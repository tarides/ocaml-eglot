;;; ocaml-eglot-util.el --- Auxiliary tools   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2024  Xavier Van de Woestyne
;; Licensed under the MIT license.

;; Author: Xavier Van de Woestyne <xaviervdw@gmail.com>
;; Created: 20 September 2024
;; Version: 1.0
;; Keywords: ocaml languages
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/tarides/ocaml-eglot

;;; Commentary:

;; Set of utilities relating to, among other things, buffer
;; manipulation to implement the ocaml-eglot functions.  This module is
;; internal and part of the ocaml-eglot project.  An add-on to the
;; Emacs Eglot LSP client for editing OCaml code.

;;; Code:

(require 'eglot)
(require 'cl-lib)

(defgroup ocaml-eglot-util nil
  "OCaml-eglot plugin standard library."
  :link '(url-link "https://ocaml.org")
  :group 'languages
  :prefix "ocaml-eglot-util-")

;; Generic util

(defun ocaml-eglot-util--vec-first-or-nil (vec)
  "Return the first element of VEC or nil."
  (when (> (length vec) 0)
    (aref vec 0)))

(defun ocaml-eglot-util--replace-region (range content)
  "Replace a LSP region (RANGE) by a given CONTENT."
  (pcase-let ((`(,beg . ,end) (eglot--range-region range)))
    (delete-region beg end)
    (goto-char beg)
    (insert content)))

(defun ocaml-eglot-util--jump-to (position)
  "Move the cursor to a POSITION calculated by LSP."
  (goto-char (eglot--lsp-position-to-point position)))

(defun ocaml-eglot-util--jump-to-range (range)
  "Move the cursor to the start of a RANGE calculated by LSP."
  (let ((start (cl-getf range :start)))
    (goto-char (eglot--lsp-position-to-point start))))

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

;; Jump features

(defun ocaml-eglot-util--extract-jump-position (jump-result)
  "Extracts the position of a JUMP-RESULT of the LSP server."
  (let ((target-vec (cl-getf jump-result :jumps)))
    (when (> (length target-vec) 0)
      (let ((real-target (aref target-vec 0)))
        (cl-getf real-target :position)))))

(provide 'ocaml-eglot-util)
;;; ocaml-eglot-util.el ends here
