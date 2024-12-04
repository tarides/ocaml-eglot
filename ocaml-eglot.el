;;; ocaml-eglot.el --- An OCaml companion for Eglot   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2024  Xavier Van de Woestyne
;; Licensed under the MIT license.

;; Author: Xavier Van de Woestyne <xaviervdw@gmail.com>
;; Created: 20 September 2024
;; Version: 1.0
;; Keywords: ocaml languages
;; Package-Requires: ((emacs "29.0"))
;; URL: https://github.com/tarides/ocaml-eglot

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

(defcustom ocaml-eglot-type-search-include-doc t
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

;;; Faces

(defface ocaml-eglot-value-name-face
  '((t :inherit font-lock-function-name-face))
  "Face describing the names of values (used for search for example)."
  :group 'ocaml-eglot)

(defface ocaml-eglot-value-type-face
  '((t :inherit font-lock-doc-face))
  "Face describing the types of values (used for search for example)."
  :group 'ocaml-eglot)

(defface ocaml-eglot-value-doc-face
  '((t :inherit font-lock-comment-face))
  "Face describing the doc of values (used for search for example)."
  :group 'ocaml-eglot)

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
          (ocaml-eglot-util--jump-to hole-start))))))

(defun ocaml-eglot-prev-hole ()
  "Jump to the previous hole."
  (interactive)
  (eglot--server-capable-or-lose :experimental :ocamllsp :handleTypedHoles)
  (let* ((current-pos (eglot--pos-to-lsp-position))
         (holes (reverse (ocaml-eglot-req--holes)))
         (hole (ocaml-eglot--first-hole-at holes current-pos '<)))
    (when hole (ocaml-eglot-util--jump-to-range hole))))

(defun ocaml-eglot-next-hole ()
  "Jump to the next hole."
  (interactive)
  (eglot--server-capable-or-lose :experimental :ocamllsp :handleTypedHoles)
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

(defun ocaml-eglot--search-as-key (value-name value-type value-doc)
  "Format a search entry with the right colours."
  (let ((name (propertize value-name 'face 'ocaml-eglot-value-name-face))
        (type (propertize value-type 'face 'ocaml-eglot-value-type-face))
        (doc (propertize value-doc 'face 'ocaml-eglot-value-doc-face)))
    (concat name " : " type " " doc)))

(defun ocaml-eglot--search-as-doc (docstring)
  "If the documentation is present, keep only the first line."
  (let* ((doc (or (and docstring (cl-getf docstring :value)) ""))
         (line (split-string doc "[\r\n]+")))
    (car line)))

(defun ocaml-eglot--search-to-completion (entries)
  "Transforms a list of entries into a search candidate (autocomplete)."
  (mapcar
   (lambda (entry)
     (let* ((value-name (cl-getf entry :name))
           (value-type (cl-getf entry :typ))
           (value-hole (cl-getf entry :constructible))
           (value-doc (ocaml-eglot--search-as-doc (cl-getf entry :doc)))
           (key (ocaml-eglot--search-as-key value-name value-type value-doc)))
       (cons key value-hole)))
   entries))

(defun ocaml-eglot--search-completion (choices selected)
  "Hook completion for keeping entries ordered by score."
  (alist-get selected choices nil nil #'equal))

(defun ocaml-eglot--search-complete-sort (choices)
  "Keeps returned search entries ordered by score."
  (lambda (string pred action)
    (if (eq action 'metadata)
	'(metadata (display-sort-function . identity)
		   (cycle-sort-function . identity))
      (complete-with-action action choices string pred))))

(defun ocaml-eglot-search (query &optional limit)
  "Search a value using his type (or polarity), the universal prefix argument
can be used to change the maximim number of result."
  (interactive "sSearch query: \np")
  (eglot--server-capable-or-lose :experimental :ocamllsp :handleTypeSearch)
  (let* ((start (eglot--pos-to-lsp-position))
         (entries (ocaml-eglot-req--search query limit))
         (choices (ocaml-eglot--search-to-completion entries))
         (chosen  (ocaml-eglot--search-completion
                   choices
                   (completing-read
                    "Candidates: "
                    (ocaml-eglot--search-complete-sort choices)
                    nil nil nil t)))
         (result (concat "(" chosen ")"))
         (end (ocaml-eglot-util--position-increase-char start result)))
    (when (region-active-p)
      (delete-region (region-beginning) (region-end)))
    (insert result)
    (ocaml-eglot--first-hole-in start end)))

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
