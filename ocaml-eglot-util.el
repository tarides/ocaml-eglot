;;; ocaml-eglot-util.el --- Auxiliary tools   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2024  Xavier Van de Woestyne
;; Licensed under the MIT license.

;; Author: Xavier Van de Woestyne <xaviervdw@gmail.com>
;; Created: 20 September 2024
;; Version: 1.0
;; Keywords: ocaml languages
;; Package-Requires: ((emacs "29.0"))
;; URL: https://github.com/tarides/ocaml-eglot

;;; Commentary

;; Set of utilities relating to, among other things, buffer
;; manipulation to implement the ocaml-eglot functions. This module is
;; internal and part of the ocaml-eglot project. An add-on to the
;; Emacs Eglot LSP client for editing OCaml code.

;;; Code

(require 'eglot)
(require 'cl-lib)

(defgroup ocaml-eglot-util nil
  "ocaml-eglot plugin standard library."
  :link '(url-link "https://ocaml.org")
  :group 'languages
  :prefix "ocaml-eglot-util-")

;; Generic util

(defun ocaml-eglot-util--replace-region (range content)
  "Replace a LSP region by a given content"
  (pcase-let ((`(,beg . ,end) (eglot--range-region range)))
    (delete-region beg end)
    (goto-char beg)
    (insert content)))

(defun ocaml-eglot-util--jump-to (lsp-position)
  "Moves the cursor to a position calculated by LSP."
  (goto-char (eglot--lsp-position-to-point lsp-position)))

(defun ocaml-eglot-util--jump-to-range (lsp-range)
  "Moves the cursor to the start of a range calculated by LSP."
  (let ((start (cl-getf lsp-range :start)))
    (goto-char (eglot--lsp-position-to-point start))))

(defun ocaml-eglot-util--compare-position (a b)
  "Comparison between two LSP positions."
  (let ((char-a (cl-getf a :character))
        (char-b (cl-getf b :character))
        (line-a (cl-getf a :line))
        (line-b (cl-getf b :line)))
    (if (> line-a line-b) 1
      (if (> line-b line-a) -1
        (if (> char-a char-b) 1
          (if (> char-b char-a) -1 0))))))

(defun ocaml-eglot-util--position-increase-char (pos content)
  "Calculates a new position after inserting text content."
  (let* ((line (cl-getf pos :line))
         (character (cl-getf pos :character))
         (new-char (+ character (length content))))
    `(:line ,line :character ,new-char)))

;; Jump features

(defun ocaml-eglot-util--extract-jump-position (jump-result)
  "Extracts the position of a jump from the result of the LSP server."
  (let ((target-vec (cl-getf jump-result :jumps)))
    (when (> (length target-vec) 0)
      (let ((real-target (aref target-vec 0)))
        (cl-getf real-target :position)))))

(provide 'ocaml-eglot-util)
;;; ocaml-eglot-util ends here
