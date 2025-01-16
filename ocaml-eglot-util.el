;;; ocaml-eglot-util.el --- Auxiliary tools   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2024-2025  Xavier Van de Woestyne
;; Licensed under the MIT license.

;; Author: Xavier Van de Woestyne <xaviervdw@gmail.com>
;; Created: 20 September 2024
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Set of utilities relating to, among other things, buffer
;; manipulation to implement the ocaml-eglot functions.  This module is
;; internal and part of the ocaml-eglot project.  An add-on to the
;; Emacs Eglot LSP client for editing OCaml code.

;;; Code:

(require 'json)
(require 'eglot)
(require 'cl-lib)

;; Generic util

(defun ocaml-eglot-util--goto-char (target)
  "Goto the point TARGET."
  (when (or (< target (point-min))
            (> target (point-max)))
    (widen))
  (goto-char target))

(defun ocaml-eglot-util--text-less-than (text limit)
  "Return non-nil if TEXT is less than LIMIT."
  (let ((count 0)
        (pos   0))
    (save-match-data
      (while (and (<= count limit)
                  (string-match "\n" text pos))
        (setq pos (match-end 0))
        (setq count (1+ count))))
    (<= count limit)))

(defun ocaml-eglot-util--vec-first-or-nil (vec)
  "Return the first element of VEC or nil."
  (when (> (length vec) 0)
    (aref vec 0)))

(defun ocaml-eglot-util--load-uri (uri)
  "Check and load if URI is available for typechecking."
  (let ((path (eglot--uri-to-path uri)))
    (when (file-exists-p path)
      (if (member path (mapcar #'buffer-file-name (buffer-list)))
          t
        (let ((buf (current-buffer)))
          (find-file path)
          (switch-to-buffer buf)
          t)))))

(defun ocaml-eglot-util-point-as-arg (point)
  "Compute POINT as a valid Merlin position."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char point)
      (let ((line (line-number-at-pos))
            (column (- (position-bytes (point))
                       (position-bytes (line-beginning-position)))))
        (format "%d:%d" line column)))))

(defun ocaml-eglot-util--point-by-pos (line col)
  "Compute LINE and COL as a point."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- line))
      (let* ((offset-l (position-bytes (point)))
             (offset-c (max 0 col))
             (target (+ offset-l offset-c)))
        (byte-to-position target)))))

(defun ocaml-eglot-util--replace-region (range content)
  "Replace a LSP region (RANGE) by a given CONTENT."
  (pcase-let ((`(,beg . ,end) (eglot--range-region range)))
    (delete-region beg end)
    (ocaml-eglot-util--goto-char beg)
    (insert content)))

(defun ocaml-eglot-util--jump-to (position)
  "Move the cursor to a POSITION calculated by LSP."
  (ocaml-eglot-util--goto-char (eglot--lsp-position-to-point position)))

(defun ocaml-eglot-util--jump-to-range (range)
  "Move the cursor to the start of a RANGE calculated by LSP."
  (let ((start (cl-getf range :start)))
    (ocaml-eglot-util--goto-char (eglot--lsp-position-to-point start))))

(defun ocaml-eglot-util--compare-position (a b)
  "Comparison between two LSP positions, A and B."
  (let ((char-a (cl-getf a :character))
        (char-b (cl-getf b :character))
        (line-a (cl-getf a :line))
        (line-b (cl-getf b :line)))
    (if (> line-a line-b) 1
      (if (> line-b line-a) -1
        (if (> char-a char-b) 1
          (if (> char-b char-a) -1 0))))))

(defun ocaml-eglot-util--position-increase-char (pos content)
  "Compute a new position (POS) after inserting text CONTENT."
  (let* ((line (cl-getf pos :line))
         (character (cl-getf pos :character))
         (new-char (+ character (length content))))
    `(:line ,line :character ,new-char)))

(defun ocaml-eglot-util--current-uri ()
  "Return the uri of the document currently being visited."
  (cl-getf (eglot--TextDocumentIdentifier) :uri))

(defun ocaml-eglot-util--is-interface (uri)
  "Return non-nil if the given URI is an interface, nil otherwise."
  (let* ((file (eglot--uri-to-path uri)))
    (string-match-p "\\.\\(mli\\|rei\\|eliomi\\)\\'" file)))

(defun ocaml-eglot-util--on-interface ()
  "Return non-nil if the current URI is an interface, nil otherwise."
  (let ((uri (ocaml-eglot-util--current-uri)))
    (ocaml-eglot-util--is-interface uri)))

(defun ocaml-eglot-util--ensure-is-interface (uri)
  "Ensure that a function is called given an interface file (URI)."
  (when (not (ocaml-eglot-util--is-interface uri))
    (eglot--error "Function is only available for interfaces")))

(defun ocaml-eglot-util--ensure-interface ()
  "Ensure that a function is called on a interface file."
  (when (not (ocaml-eglot-util--on-interface))
    (eglot--error "Function is only available for interfaces")))

(defun ocaml-eglot-util--format-markup (markup)
  "Format MARKUP according to LSP's spec."
  (eglot--format-markup markup))

(defun ocaml-eglot-util--current-range ()
  "Return the current active range."
  (if (region-active-p)
      (let ((region-start (region-beginning))
            (region-stop  (region-end)))
        (list :start (eglot--pos-to-lsp-position region-start)
              :end (eglot--pos-to-lsp-position region-stop)))
    (let ((start (eglot--pos-to-lsp-position)))
      (list :start start
            :end (ocaml-eglot-util--position-increase-char start "_")))))

(defun ocaml-eglot-util--current-position-or-range ()
  "Return the current position or a range if the region is active."
  (if (region-active-p)
      (let ((beg (eglot--pos-to-lsp-position (region-beginning)))
            (end (eglot--pos-to-lsp-position (region-end))))
        `(:start ,beg :end ,end))
    (eglot--pos-to-lsp-position)))

(defun ocaml-eglot-util--visit-file (strategy current-file new-file range)
  "Visits a referenced document, NEW-FILE at position  start of RANGE.
The STRATEGY can be `'new' `'current' or `'smart'.  The later opens a
new window if the destination is not in the CURRENT-FILE, ans uses the
current window otherwise."
  (push-mark)
  (cond ((eq strategy 'new) (find-file-other-window new-file))
        ((eq strategy 'current) (find-file new-file))
        ((string= current-file new-file) (find-file new-file))
        (t (find-file-other-window new-file)))
  (ocaml-eglot-util--jump-to-range range))

(defun ocaml-eglot-util--highlight-range (range face)
  "Highlight a given RANGE using a given FACE."
  (remove-overlays nil nil 'ocaml-eglot-highlight 'highlight)
  (let* ((beg (eglot--lsp-position-to-point (cl-getf range :start)))
        (end (eglot--lsp-position-to-point (cl-getf range :end)))
        (overlay (make-overlay beg end)))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'ocaml-eglot-highlight 'highlight)
    (unwind-protect (sit-for 60) (delete-overlay overlay))))

(defun ocaml-eglot-util--as-json (str)
  "Parse a string STR as a Json object."
  (json-parse-string str :object-type 'plist))

(defun ocaml-eglot-util--merlin-call-result (result)
  "Extract the RESULT of a Merlin Call Compatible request."
  (let* ((result (cl-getf result :result))
         (json-result (ocaml-eglot-util--as-json result))
         (result-class (cl-getf json-result :class)))
    (if (string= result-class "return")
        (cl-getf json-result :value)
      (eglot--error "Invalid result class %s" result-class))))

(provide 'ocaml-eglot-util)
;;; ocaml-eglot-util.el ends here
