;;; ocaml-eglot.el --- An OCaml companion for Eglot   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2024  Xavier Van de Woestyne
;; Licensed under the MIT license.

;; Author: Xavier Van de Woestyne <xaviervdw@gmail.com>
;; Created: 20 September 2024
;; Version: 1.0
;; Keywords: ocaml languages
;; Package-Requires: ((emacs "29.0"))
;; URL: https://github.com/xvw/ocaml-eglot

;;; Commentary

;; TODO: Write a commentary

;;; Code


(require 'cl-lib)
(require 'eglot)
(require 'ocaml-eglot-util)
(require 'ocaml-eglot-request)
(require 'tuareg)
(require 'flymake)

(defgroup ocaml-eglot nil
  "All interactions from Eglot to OCaml-lsp-server."
  :link '(url-link "https://ocaml.org")
  :group 'languages
  :prefix "ocaml-eglot-")

;;; Customizable variables

(defcustom ocaml-eglot-type-search-limit 20
  "The limit of number of search result for type and polarity search."
  :group 'ocaml-eglot
  :type 'natnum)

(defcustom ocaml-eglot-type-search-include-doc nil
  "Include documentation in type search result."
  :group 'ocaml-eglot
  :type 'boolean)

(defcustom ocaml-eglot-construct-with-local-values nil
  "If non-nil, `merlin-construct' includes values in the local environment.
Otherwise, `merlin-construct' only includes constructors."
  :group 'ocaml-eglot
  :type 'boolean)

(defcustom ocaml-eglot-preferred-markupkind "plaintext"
  "Preferred markup format."
  :group 'ocaml-eglot
  :type 'string)

;;; Features

;; Jump to errors

(defun ocaml-eglot-error-next ()
  "Jump to the next error."
  (interactive)
  (flymake-goto-next-error))

(defun ocaml-eglot-error-prev ()
  "Jump to the previous error."
  (interactive)
  (flymake-goto-prev-error))

;; Holes

;; TODO: It would probably be possible to improve the query to embed
;; more logic at the `merlin-lib` level rather than calculating hole
;; logic at the editor level.

(defun ocaml-eglot--first-hole-aux (holes pos comparison)
  "Returns the first HOLE of the list according to a comparison predicate."
  (when (or holes (> 0 (length holes)))
    (let* ((hd (car holes))
           (tl (cdr holes))
           (h-start (cl-getf hd :start))
           (cmp (ocaml-eglot-util--compare-position h-start pos)))
      (if (funcall comparison cmp 0) hd
        (ocaml-eglot--first-hole-aux tl pos comparison)))))

(defun ocaml-eglot--first-hole-at (holes pos comparison)
  "Returns the first HOLE of the list according to a comparison predicate.
If there is not valid hole, the first hole of the list is returned."
  (let ((hole (ocaml-eglot--first-hole-aux holes pos comparison)))
    (if hole hole (car holes))))

(defun ocaml-eglot--first-hole-in (start end)
  "Jump to the first hole in a given range."
  (let* ((holes (ocaml-eglot-req--holes))
         (hole (ocaml-eglot--first-hole-at holes start '>)))
    (when hole
      (let ((hole-start (cl-getf hole :start))
            (hole-end (cl-getf hole :end)))
        (when (and (>= (ocaml-eglot-util--compare-position hole-start start) 0)
                   (<= (ocaml-eglot-util--compare-position hole-end end) 0))
          (progn (ocaml-eglot-util--jump-to hole-start)))))))

(defun ocaml-eglot-prev-hole ()
  "Jump to the previous hole."
  (interactive)
  (let* ((current-pos (eglot--pos-to-lsp-position))
         (holes (reverse (ocaml-eglot-req--holes)))
         (hole (ocaml-eglot--first-hole-at holes current-pos '<)))
    (when hole (ocaml-eglot-util--jump-to-range hole))))

(defun ocaml-eglot-next-hole ()
  "Jump to the next hole."
  (interactive)
  (let* ((current-pos (eglot--pos-to-lsp-position))
         (holes (ocaml-eglot-req--holes))
         (hole (ocaml-eglot--first-hole-at holes current-pos '>)))
    (when hole (ocaml-eglot-util--jump-to-range hole))))

;; Jump to source elements

(defun ocaml-eglot-jump (target)
  "Jumps to the begining of the closest fun/let/match/module/module-type"
  (interactive
   (list (completing-read
          "target: "
          '("fun" "let" "match" "module"
            "module-type" "match-next-case"
            "match-prev-case"))))
  (eglot--server-capable-or-lose :experimental :ocamllsp :handleJump)
  (let* ((jumps (ocaml-eglot-req--jump target))
         (position (ocaml-eglot-util--extract-jump-position jumps)))
    (unless position (eglot--error "No matching target"))
    (ocaml-eglot-util--jump-to position)))

;; Search by type or polarity

(defun ocaml-eglot-search (query &optional limit)
  "Search a value using his type (or polarity), the universal prefix argument
can be used to change the maximim number of result."
  (interactive "sSearch query: \np")
  (eglot--server-capable-or-lose :experimental :ocamllsp :handleTypeSearch)
  (print (ocaml-eglot-req--search query limit)))

;; Construct

(defun ocaml-eglot-construct (&optional prefix-arg)
  "Construct over the current hole."
  (interactive "P")
  (eglot--server-capable-or-lose :experimental :ocamllsp :handleConstruct)
  (let* ((result (ocaml-eglot-req--construct 0 prefix-arg))
         (range (cl-getf result :position))
         (suggestions (append (cl-getf result :result) nil)))
    (when (= (length suggestions) 0)
      (eglot--error "No constructors for this hole"))
    (cl-labels
        ((insert-construct-choice (subst)
           (let* ((start (cl-getf range :start))
                  (end (ocaml-eglot-util--position-increase-char
                        start subst)))
             (ocaml-eglot-util--replace-region range subst)
             (ocaml-eglot--first-hole-in start end))))
      (if (= (length suggestions) 1)
          (insert-construct-choice (car suggestions))
        (let ((choice (completing-read "Constructor: " suggestions nil t)))
          (insert-construct-choice choice))))))


;;; Mode

(defvar ocaml-eglot-map
  (let ((ocaml-eglot-keymap (make-sparse-keymap)))
    (define-key ocaml-eglot-keymap (kbd "C-c C-x") #'ocaml-eglot-error-next)
    (define-key ocaml-eglot-keymap (kbd "C-c C-c") #'ocaml-eglot-error-prev)
    ocaml-eglot-keymap)
  "Keymap for OCaml-eglot minor mode")

;;;###autoload
(define-minor-mode ocaml-eglot
  "TODO: I will write a better documentation"
  :init-value nil
  :lighter " ocaml-eglot"
  :keymap ocaml-eglot-map)


(provide 'ocaml-eglot)
;;; ocaml-eglot ends here
