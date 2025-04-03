;;; ocaml-eglot-type-enclosing.el --- Type Enclosing feature   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2024-2025  Xavier Van de Woestyne
;; Licensed under the MIT license.

;; Author: Xavier Van de Woestyne <xaviervdw@gmail.com>
;; Created: 10 January 2025
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Plumbing needed to implement the primitives related to type
;; enclosing commands.

;;; Code:

(require 'cl-lib)
(require 'ocaml-eglot-util)
(require 'ocaml-eglot-req)

;;; Customizable variables

(defcustom ocaml-eglot-type-buffer-name "*ocaml-eglot-types*"
  "The name of the buffer storing types."
  :group 'ocaml-eglot
  :type 'string)

;;; Internal variables

(defvar-local ocaml-eglot-type-enclosing-types nil
  "Current list of enclosings related to types.")

(defvar-local ocaml-eglot-type-enclosing-current-type nil
  "Current type for the current enclosing.")

(defvar-local ocaml-eglot-type-enclosing-offset 0
  "The offset of the requested enclosings.")

(defvar-local ocaml-eglot-type-enclosing-verbosity 0
  "The verbosity of the current enclosing request.")

;;; Key mapping for type enclosing

(defvar ocaml-eglot-type-enclosing-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-<up>") #'ocaml-eglot-type-enclosing-grow)
    (define-key keymap (kbd "C-<down>") #'ocaml-eglot-type-enclosing-shrink)
    (define-key keymap (kbd "C-w") #'ocaml-eglot-type-enclosing-copy)
    (define-key keymap (kbd "C-c C-t") #'ocaml-eglot-type-enclosing-increase-verbosity)
    (define-key keymap (kbd "C-<right>") #'ocaml-eglot-type-enclosing-increase-verbosity)
    (define-key keymap (kbd "C-<left>") #'ocaml-eglot-type-enclosing-decrease-verbosity)
    keymap)
  "Keymap for OCaml-eglot's type enclosing transient mode.")

;;; Internal functions

(defun ocaml-eglot-type-enclosing-copy ()
  "Copy the type of the current enclosing to the Kill-ring."
  (interactive)
  (when ocaml-eglot-type-enclosing-current-type
    (eglot--message "Copied `%s' to kill-ring"
                    ocaml-eglot-type-enclosing-current-type)
    (kill-new ocaml-eglot-type-enclosing-current-type)))

(defun ocaml-eglot-type-enclosing--with-fixed-offset (&optional prev-verb)
  "Compute the type enclosing for a dedicated offset.
If PREV-VERB is given, the verbosity change ensure that the type is different."
  (let* ((verbosity ocaml-eglot-type-enclosing-verbosity)
         (index ocaml-eglot-type-enclosing-offset)
         (at (ocaml-eglot-util--current-position-or-range))
         (result (ocaml-eglot-req--type-enclosings at index verbosity))
         (type (cl-getf result :type)))
    (when (and prev-verb
               (string= type ocaml-eglot-type-enclosing-current-type))
      (setq ocaml-eglot-type-enclosing-verbosity prev-verb))
    (setq ocaml-eglot-type-enclosing-current-type type)
    (ocaml-eglot-type-enclosing--display type t)))

(defun ocaml-eglot-type-enclosing-increase-verbosity ()
  "Increase the verbosity of the current request."
  (interactive)
  (let ((prev-verbosity ocaml-eglot-type-enclosing-verbosity))
    (setq ocaml-eglot-type-enclosing-verbosity
          (1+ ocaml-eglot-type-enclosing-verbosity))
    (ocaml-eglot-type-enclosing--with-fixed-offset prev-verbosity)))

(defun ocaml-eglot-type-enclosing-decrease-verbosity ()
  "Decrease the verbosity of the current request."
  (interactive)
  (when (> ocaml-eglot-type-enclosing-verbosity 0)
    (setq ocaml-eglot-type-enclosing-verbosity
          (1- ocaml-eglot-type-enclosing-verbosity)))
  (ocaml-eglot-type-enclosing--with-fixed-offset))

(defun ocaml-eglot-type-enclosing-grow ()
  "Growing of the type enclosing."
  (interactive)
  (when ocaml-eglot-type-enclosing-types
    (setq ocaml-eglot-type-enclosing-offset
          (mod (1+ ocaml-eglot-type-enclosing-offset)
               (length ocaml-eglot-type-enclosing-types)))
    (ocaml-eglot-type-enclosing--with-fixed-offset)))

(defun ocaml-eglot-type-enclosing-shrink ()
  "Display the type enclosing of a smaller enclosing if possible."
  (interactive)
  (when ocaml-eglot-type-enclosing-types
    (setq ocaml-eglot-type-enclosing-offset
          (mod (1- ocaml-eglot-type-enclosing-offset)
               (length ocaml-eglot-type-enclosing-types)))
    (ocaml-eglot-type-enclosing--with-fixed-offset)))

(defun ocaml-eglot-type-enclosing--type-buffer (type-expr)
  "Create buffer with content TYPE-EXPR of the enclosing type buffer."
  ; We store the current major mode to be used in the type buffer for
  ; syntax highlighting.
  (let ((curr-dir default-directory)
        (current-major-mode major-mode))
    (with-current-buffer (get-buffer-create ocaml-eglot-type-buffer-name)
      (read-only-mode 0)
      (funcall current-major-mode)
      (erase-buffer)
      (insert type-expr)
      (goto-char (point-min))
      (read-only-mode 1)
      (setq default-directory curr-dir))))

(defun ocaml-eglot-type-enclosing--display (type-expr &optional current)
  "Display the type-enclosing for TYPE-EXPR in a dedicated buffer.
If CURRENT is set, the range of the enclosing will be highlighted."
  (ocaml-eglot-type-enclosing--type-buffer type-expr)
  (if (ocaml-eglot-util--text-less-than type-expr 8)
      (message "%s" (with-current-buffer ocaml-eglot-type-buffer-name
                      (font-lock-fontify-region (point-min) (point-max))
                      (buffer-string)))
    (display-buffer ocaml-eglot-type-buffer-name))
  (when (and current (not (equal [] ocaml-eglot-type-enclosing-types)))
    (let ((current (aref ocaml-eglot-type-enclosing-types
                                   ocaml-eglot-type-enclosing-offset)))
      (ocaml-eglot-util--highlight-range current
                                         'ocaml-eglot-highlight-region-face))))

(defun ocaml-eglot-type-enclosing--reset ()
  "Reset local variables defined by the enclosing query."
  (setq ocaml-eglot-type-enclosing-current-type nil)
  (setq ocaml-eglot-type-enclosing-verbosity 0)
  (setq ocaml-eglot-type-enclosing-types nil)
  (setq ocaml-eglot-type-enclosing-offset 0))

(defun ocaml-eglot-type-enclosing--call ()
  "Print the type of the expression under point."
  (ocaml-eglot-type-enclosing--reset)
  (let* ((verbosity ocaml-eglot-type-enclosing-verbosity)
         (index ocaml-eglot-type-enclosing-offset)
         (at (ocaml-eglot-util--current-position-or-range))
         (result (ocaml-eglot-req--type-enclosings at index verbosity))
         (type (cl-getf result :type))
         (enclosings (cl-getf result :enclosings)))
    (setq ocaml-eglot-type-enclosing-types enclosings)
    (setq ocaml-eglot-type-enclosing-current-type type)
    (ocaml-eglot-type-enclosing--display type t)
    (set-transient-map ocaml-eglot-type-enclosing-map t
                       'ocaml-eglot-type-enclosing--reset)))

(provide 'ocaml-eglot-type-enclosing)
;;; ocaml-eglot-type-enclosing.el ends here
