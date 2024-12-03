;;; ocaml-eglot-util.el --- Auxiliary tools   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2024  Xavier Van de Woestyne
;; Licensed under the MIT license.

;; Author: Xavier Van de Woestyne <xaviervdw@gmail.com>
;; Created: 20 September 2024
;; Version: 1.0
;; Keywords: ocaml languages
;; Package-Requires: ((emacs "29.0"))
;; URL: https://github.com/xvw/ocaml-eglot

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

;; Jump features

(defun ocaml-eglot-util--extract-jump-position (jump-result)
  "Extracts the position of a jump from the result of the LSP server."
  (let ((target-vec (cl-getf jump-result :jumps)))
    (when (> (length target-vec) 0)
      (let ((real-target (aref target-vec 0)))
        (cl-getf real-target :position)))))

;; Search by types utils

(provide 'ocaml-eglot-util)
;;; ocaml-eglot-util ends here
