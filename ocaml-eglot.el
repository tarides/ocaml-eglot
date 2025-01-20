;;; ocaml-eglot.el --- An OCaml companion for Eglot   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2024-2025  The OCaml-eglot Project Contributors
;; Licensed under the MIT license.

;; Author: Xavier Van de Woestyne <xaviervdw@gmail.com>
;;         Frédéric Bour
;;         Simon Castellan
;;         Thomas Refis
;;         Ulysse Gerard
;; Maintainer: Xavier Van de Woestyne <xaviervdw@gmail.com>
;; Created: 20 September 2024
;; Keywords: ocaml languages
;; URL: https://github.com/tarides/ocaml-eglot
;; Package-Requires: ((emacs "29.1"))
;; Package-Version: 0.1
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provides a development environment for writing OCaml code.  Built on
;; top of `ocaml-lsp-server`
;; (https://ocaml.org/p/ocaml-lsp-server/latest) via Eglot
;; (https://www.gnu.org/software/emacs/manual/html_mono/eglot.html)
;; for LSP interactions.  `ocaml-eglot` provides standard
;; implementations of the various custom-requests exposed by
;; `ocaml-lsp-server`.

;; Under the bonnet, `ocaml-eglot` and `ocaml-lsp-server` take
;; advantage of `merlin` (https://ocaml.org/p/merlin-lib/latest) as a
;; library to provide advanced IDE services.

;;; Code:

(require 'flymake)
(require 'xref)
(require 'cl-lib)
(require 'ocaml-eglot-util)
(require 'ocaml-eglot-req)
(require 'ocaml-eglot-type-enclosing)
(require 'eglot)

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

(defcustom ocaml-eglot-preferred-markupkind "markdown"
  "Preferred markup format."
  :group 'ocaml-eglot
  :type 'string)

(defcustom ocaml-eglot-open-window-strategy 'smart
  "Defines window opening strategy."
  :group 'ocaml-eglot
  :type '(choice (const :tag "Open a new window only if the target differs" smart)
                 (const :tag "Always open in a new window" new)
                 (const :tag "Always open in the current window" current)))

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

(defface ocaml-eglot-highlight-region-face
  '((t (:inherit highlight)))
  "Face used when highlighting a region.")

;;; Features

;; Jump to errors

(defun ocaml-eglot-error-next ()
  "Jump to the next error."
  (interactive)
  (call-interactively #'flymake-goto-next-error))

(defun ocaml-eglot-error-prev ()
  "Jump to the previous error."
  (interactive)
  (call-interactively #'flymake-goto-prev-error))

;; Jump to definition

(defun ocaml-eglot--find-definition (strategy)
  "Find the definition at point and jump to it using STRATEGY."
  (let* ((query-result (ocaml-eglot-req--definition))
         (result (ocaml-eglot-util--vec-first-or-nil query-result)))
    (if result
        (let* ((uri (cl-getf result :uri))
               (range (cl-getf result :range))
               (file (eglot--uri-to-path uri)))
          (ocaml-eglot-util--visit-file strategy (buffer-file-name) file range))
      (eglot--error "Not in environment"))))

(defun ocaml-eglot-find-definition ()
  "Find the definition identifier at point."
  (interactive)
  (ocaml-eglot--find-definition ocaml-eglot-open-window-strategy))

(defun ocaml-eglot-find-definition-in-new-window ()
  "Find the definition of the identifier at point and show it in a new window."
  (interactive)
  (ocaml-eglot--find-definition 'new))

(defun ocaml-eglot-find-definition-in-current-window ()
  "Find the definition of the identifier and show it in the current window."
  (interactive)
  (ocaml-eglot--find-definition 'current))

;; Jump to declaration

(defun ocaml-eglot--find-declaration (strategy)
  "Find the declaration of the identifier at point and jump to it using STRATEGY."
  (let* ((query-result (ocaml-eglot-req--declaration))
         (result (ocaml-eglot-util--vec-first-or-nil query-result)))
    (if result
        (let* ((uri (cl-getf result :uri))
               (range (cl-getf result :range))
               (file (eglot--uri-to-path uri)))
          (ocaml-eglot-util--visit-file strategy (buffer-file-name) file range))
      (eglot--error "Not in environment"))))

(defun ocaml-eglot-find-declaration ()
  "Find the declaration of the identifier at point."
  (interactive)
  (ocaml-eglot--find-declaration ocaml-eglot-open-window-strategy))

(defun ocaml-eglot-find-declaration-in-new-window ()
  "Find the declaration of the identifier at point and show it in a new window."
  (interactive)
  (ocaml-eglot--find-declaration 'new))

(defun ocaml-eglot-find-declaration-in-current-window ()
  "Find the declaration of the identifier at point.
Show it the current window."
  (interactive)
  (ocaml-eglot--find-declaration 'current))

;; Jump type declaration of expression

(defun ocaml-eglot--find-type-definition (strategy)
  "Find the definition of the type of the expression at point using STRATEGY."
  (let* ((query-result (ocaml-eglot-req--type-definition))
         (result (ocaml-eglot-util--vec-first-or-nil query-result)))
    (if result
        (let* ((uri (cl-getf result :uri))
               (range (cl-getf result :range))
               (file (eglot--uri-to-path uri)))
          (ocaml-eglot-util--visit-file strategy (buffer-file-name) file range))
      (eglot--error "Not in environment"))))

(defun ocaml-eglot-find-type-definition ()
  "Find the definition of the type of the expression at point."
  (interactive)
  (ocaml-eglot--find-type-definition ocaml-eglot-open-window-strategy))

(defun ocaml-eglot-find-type-definition-in-new-window ()
  "Find the definition of the type of the expression at point.
Show it in a new window."
  (interactive)
  (ocaml-eglot--find-type-definition 'new))

(defun ocaml-eglot-find-type-definition-in-current-window ()
  "Find the definition of the type of the expression at point.
Show it in the current window."
  (interactive)
  (ocaml-eglot--find-type-definition 'current))

;; Infer interface

(defun ocaml-eglot--find-alternate-file (uri)
  "Return the alternative file for a given URI."
  (let ((uris (ocaml-eglot-req--switch-file uri)))
    (ocaml-eglot-util--vec-first-or-nil uris)))

(defun ocaml-eglot-infer-interface (&optional need-confirmation)
  "Infer the interface for the current file.
If NEED-CONFIRMATION is set to non-nil, it will prompt a confirmation."
  (interactive)
  (eglot--server-capable-or-lose :experimental :ocamllsp :handleInferIntf)
  (ocaml-eglot-util--ensure-interface)
  (let* ((current-uri (ocaml-eglot-util--current-uri))
         (impl-uri (ocaml-eglot--find-alternate-file current-uri)))
    (if (ocaml-eglot-util--load-uri impl-uri)
      (when (or (not need-confirmation)
                (y-or-n-p "Try to generate interface? ") )
        (let ((result (ocaml-eglot-req--infer-intf impl-uri)))
          (when (or (= (buffer-size) 0)
                    (y-or-n-p "The buffer is not empty, overwrite it? "))
            (erase-buffer)
            (insert result))))
      (when (not need-confirmation)
        (eglot--error "%s is not loaded" impl-uri)))))

;; Find alternate file `ml<->mli'

(defun ocaml-eglot-alternate-file ()
  "Visit the alternative file (ml to mli and vice versa)."
  (interactive)
  ;; We don't relay on `tuareg-find-alternate-file‘ because the
  ;; interface generation relies on `ocamlmerlin’.
  (eglot--server-capable-or-lose :experimental :ocamllsp :handleSwitchImplIntf)
  (when-let* ((current-uri (ocaml-eglot-util--current-uri))
              (uri (ocaml-eglot--find-alternate-file current-uri))
              (file (eglot--uri-to-path uri)))
    (find-file file)))

;; Hook when visiting new interface file

(defun ocaml-eglot--file-hook ()
  "Hook to try to generate interface on visiting new files.."
  (when (and
         (ocaml-eglot-util--on-interface)
         (= (buffer-size) 0))
    (ocaml-eglot-infer-interface t )))

;; Holes

;; TODO: It would probably be possible to improve the query to embed
;; more logic at the `merlin-lib` level rather than calculating hole
;; logic at the editor level.

(defun ocaml-eglot--first-hole-aux (holes pos comparison)
  "Return the first hole of the list HOLES since a POS using COMPARISON."
  (when (or holes (> 0 (length holes)))
    (let* ((hd (car holes))
           (tl (cdr holes))
           (h-start (cl-getf hd :start))
           (cmp (ocaml-eglot-util--compare-position h-start pos)))
      (if (funcall comparison cmp 0) hd
        (ocaml-eglot--first-hole-aux tl pos comparison)))))

(defun ocaml-eglot--first-hole-at (holes pos comparison)
  "Return the first hole of the list HOLES since a POS using COMPARISON.
If there is no available holes, it returns the first one of HOLES."
  (let ((hole (ocaml-eglot--first-hole-aux holes pos comparison)))
    (if hole hole (car holes))))

(defun ocaml-eglot--get-first-hole-in (start end)
  "Return the first hole in a given range denoted by START and END."
  (let* ((holes (ocaml-eglot-req--holes))
         (hole (ocaml-eglot--first-hole-at holes start '>=)))
    (when hole
      (let ((hole-start (cl-getf hole :start))
            (hole-end (cl-getf hole :end)))
        (when (and (>= (ocaml-eglot-util--compare-position hole-start start) 0)
                   (<= (ocaml-eglot-util--compare-position hole-end end) 0))
          hole)))))

(defun ocaml-eglot--first-hole-in (start end)
  "Jump to the first hole in a given range denoted by START and END."
  (when-let ((hole (ocaml-eglot--get-first-hole-in start end))
             (hole-start (cl-getf hole :start)))
    (ocaml-eglot-util--jump-to hole-start)))

(defun ocaml-eglot-hole-prev ()
  "Jump to the previous hole."
  (interactive)
  (eglot--server-capable-or-lose :experimental :ocamllsp :handleTypedHoles)
  (let* ((current-pos (eglot--pos-to-lsp-position))
         (holes (reverse (ocaml-eglot-req--holes)))
         (hole (ocaml-eglot--first-hole-at holes current-pos '<)))
    (when hole (ocaml-eglot-util--jump-to-range hole))))

(defun ocaml-eglot-hole-next ()
  "Jump to the next hole."
  (interactive)
  (eglot--server-capable-or-lose :experimental :ocamllsp :handleTypedHoles)
  (let* ((current-pos (eglot--pos-to-lsp-position))
         (holes (ocaml-eglot-req--holes))
         (hole (ocaml-eglot--first-hole-at holes current-pos '>)))
    (when hole (ocaml-eglot-util--jump-to-range hole))))

;; Jump to source elements

(defun ocaml-eglot-jump ()
  "Jumps to the the closest fun/let/match/module/module-type/match-case."
  (interactive)
  (eglot--server-capable-or-lose :experimental :ocamllsp :handleJump)
  (let ((jumps-result (cl-getf (ocaml-eglot-req--jump) :jumps)))
    (when (<= (length jumps-result) 0)
      (eglot--error "No matching target"))
    (let* ((jumps
            (mapcar
             (lambda (jump)
               (let ((key (cl-getf jump :target))
                     (value (cl-getf jump :position)))
                 (cons key value)))
             jumps-result))
           (selected (completing-read "Target: " jumps))
           (position (alist-get selected jumps nil nil #'string-equal)))
      (if position
          (ocaml-eglot-util--jump-to position)
        (eglot--error "Target not found")))))

(defun ocaml-eglot--phrase (direction)
  "Move to the next or previous phrase using DIRECTION."
  (let* ((result (ocaml-eglot-req--phrase direction))
         (json-result (ocaml-eglot-util--merlin-call-result result))
         (pos (cl-getf json-result :pos)))
    (when pos
      (let* ((line (cl-getf pos :line))
             (col (cl-getf pos :col))
             (target (ocaml-eglot-util--point-by-pos line col)))
        (ocaml-eglot-util--goto-char target)))))

(defun ocaml-eglot-phrase-next ()
  "Go to the beginning of the next phrase."
  (interactive)
  (ocaml-eglot--phrase "next"))

(defun ocaml-eglot-phrase-prev ()
  "Go to the beginning of the previous phrase."
  (interactive)
  (ocaml-eglot--phrase "prev"))

;; Search by type or polarity

(defun ocaml-eglot--search-as-key (value-name value-type value-doc)
  "Format a search entry given a VALUE-NAME, a VALUE-TYPE and a VALUE-DOC."
  (let ((name (propertize value-name 'face 'ocaml-eglot-value-name-face))
        (type (propertize value-type 'face 'ocaml-eglot-value-type-face))
        (doc (propertize value-doc 'face 'ocaml-eglot-value-doc-face)))
    (concat name " : " type " " doc)))

(defun ocaml-eglot--search-as-doc (docstring)
  "If the documentation (using DOCSTRING) is present, keep only the first line."
  (let* ((doc (or (and docstring (cl-getf docstring :value)) ""))
         (line (split-string doc "[\r\n]+")))
    (car line)))

(defun ocaml-eglot--search-to-completion (entries key-completable)
  "Transforms a list of ENTRIES into a search candidate (autocomplete).
KEY-COMPLETABLE define the current value to be selected."
  (mapcar
   (lambda (entry)
     (let* ((value-name (cl-getf entry :name))
            (value-type (cl-getf entry :typ))
            (value-hole (cl-getf entry key-completable))
            (value-doc (ocaml-eglot--search-as-doc (cl-getf entry :doc)))
            (key (ocaml-eglot--search-as-key value-name value-type value-doc)))
       (cons key value-hole)))
   entries))

(defun ocaml-eglot--search-completion (choices selected)
  "Hook completion (SELECTED) for keeping CHOICES ordered by score."
  (alist-get selected choices nil nil #'equal))

(defun ocaml-eglot--search-complete-sort (choices)
  "Keep returned search entries (CHOICES) ordered by score."
  (lambda (string pred action)
    (if (eq action 'metadata)
	'(metadata (display-sort-function . identity)
		   (cycle-sort-function . identity))
      (complete-with-action action choices string pred))))

(defun ocaml-eglot--search (query limit key)
  "Search a value using his type (or polarity) by a QUERY.
the universal prefix argument can be used to change the maximum number
of result (LIMIT).  KEY define the current value to be selected."
  (eglot--server-capable-or-lose :experimental :ocamllsp :handleTypeSearch)
  (let* ((limit (or(if (> limit 1) limit nil)
                   ocaml-eglot-type-search-limit 25))
         (with-doc (or ocaml-eglot-type-search-include-doc :json-false))
         ;; We use plaintext because the result of the documentation may
         ;; be truncated
         (entries (ocaml-eglot-req--search query limit with-doc "plaintext"))
         (choices (ocaml-eglot--search-to-completion entries key))
         (chosen  (ocaml-eglot--search-completion
                   choices
                   (completing-read
                    "Candidates: "
                    (ocaml-eglot--search-complete-sort choices)
                    nil nil nil t))))
    chosen))

(defun ocaml-eglot-search (query &optional limit)
  "Search a value using his type (or polarity) by a QUERY.
the universal prefix argument can be used to change the maximim number
of result (LIMIT)."
  (interactive "sSearch query: \np")
  (eglot--server-capable-or-lose :experimental :ocamllsp :handleTypeSearch)
  (let* ((start (eglot--pos-to-lsp-position))
         (chosen (ocaml-eglot--search query limit :constructible))
         (result (concat "(" chosen ")"))
         (end (ocaml-eglot-util--position-increase-char start result)))
    (when (region-active-p)
      (delete-region (region-beginning) (region-end)))
    (insert result)
    (ocaml-eglot--first-hole-in start end)))

;; Construct

(defun ocaml-eglot--construct-local-values (with-local-value)
  "Constructs WITH-LOCAL-VALUE parameter for the `construct’ query."
  (if (or with-local-value ocaml-eglot-construct-with-local-values)
      "local"
    "none"))

(defun ocaml-eglot--construct-with-hole (arg hole)
  "Construct over the current HOLE.
It use the ARG to use local values or not."
  (let* ((with-local-value (ocaml-eglot--construct-local-values arg))
         (hole-start (cl-getf hole :start))
         (result (ocaml-eglot-req--construct hole-start 1 with-local-value))
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

(defun ocaml-eglot-construct (&optional arg)
  "Construct over the current hole or insert-it.
It use the ARG to use local values or not."
  (interactive "P")
  (eglot--server-capable-or-lose :experimental :ocamllsp :handleConstruct)
  (let* ((current-range (ocaml-eglot-util--current-range))
         (start (cl-getf current-range :start))
         (end (cl-getf current-range :end))
         (hole (ocaml-eglot--get-first-hole-in start end)))
    (if hole
        (ocaml-eglot--construct-with-hole arg hole)
      (if (not (equal (symbol-at-point) '_))
          (progn (save-excursion (insert "_"))
                 (let ((hole (ocaml-eglot--get-first-hole-in start end)))
                   (if (not hole)
                       (progn (delete-char 1)
                              (eglot--error "Not a hole"))
                     (ocaml-eglot--construct-with-hole arg hole))))))))

;; Get Documentation

(defun ocaml-eglot--document-aux (identifier)
  "Displays the OCaml documentation for the IDENTIFIER under the cursor."
  (when-let* ((result (ocaml-eglot-req--get-documentation
                       identifier ocaml-eglot-preferred-markupkind))
              (doc-value (cl-getf result :doc))
              (formated-value (ocaml-eglot-util--format-markup doc-value)))
    (message "%s" formated-value)))

(defun ocaml-eglot-document ()
  "Displays the OCaml documentation for the identifier under the cursor."
  (interactive)
  (ocaml-eglot--document-aux nil))

(defun ocaml-eglot-document-identifier (identifier)
  "Displays the OCaml documentation for a given IDENTIFIER."
  (interactive "sIdentifier: ")
  (ocaml-eglot--document-aux identifier))

;; Type Enclosings

(defun ocaml-eglot-type-expression (expression)
  "Prompt the user for expression EXPRESSION and print its type."
  (interactive "sExpression: ")
  (let* ((result (ocaml-eglot-req--type-expression expression))
         (type-expr (ocaml-eglot-util--merlin-call-result result)))
    (ocaml-eglot-type-enclosing--display type-expr nil)))

(defun ocaml-eglot-type-enclosing (&optional prefix)
  "Print the type of the expression under point (or of the region, if it exists).
If called repeatedly, increase the verbosity of the type shown.
if PREFIX is given, prompt the user for expression EXPRESSION
and print its type."
  (interactive "P")
  (if prefix
      (call-interactively #'ocaml-eglot-type-expression)
    (ocaml-eglot-type-enclosing--call)))


;; Case Analysis

(defun ocaml-eglot-destruct ()
  "Perform case-analysis at the current point."
  (interactive)
  (ocaml-eglot-req--destruct))

;; Occurences

(defun ocaml-eglot-occurences ()
  "Find all occurrences of the identifier under the cursor."
  (interactive)
  (call-interactively #'xref-find-references))

(defun ocaml-eglot-rename ()
  "Rename the symbol at point."
  (interactive)
  (call-interactively #'eglot-rename))

;;; Mode

(defvar ocaml-eglot-map
  (let ((ocaml-eglot-keymap (make-sparse-keymap)))
    (define-key ocaml-eglot-keymap (kbd "C-c C-x") #'ocaml-eglot-error-next)
    (define-key ocaml-eglot-keymap (kbd "C-c C-c") #'ocaml-eglot-error-prev)
    (define-key ocaml-eglot-keymap (kbd "C-c C-l") #'ocaml-eglot-find-definition)
    (define-key ocaml-eglot-keymap (kbd "C-c C-i") #'ocaml-eglot-find-declaration)
    (define-key ocaml-eglot-keymap (kbd "C-c C-a") #'ocaml-eglot-alternate-file)
    (define-key ocaml-eglot-keymap (kbd "C-c C-d") #'ocaml-eglot-document)
    (define-key ocaml-eglot-keymap (kbd "C-c C-t") #'ocaml-eglot-type-enclosing)
    (define-key ocaml-eglot-keymap (kbd "C-c |") #'ocaml-eglot-destruct)
    (define-key ocaml-eglot-keymap (kbd "C-c \\") #'ocaml-eglot-construct)
    (define-key ocaml-eglot-keymap (kbd "C-c C-p") #'ocaml-eglot-phrase-prev)
    (define-key ocaml-eglot-keymap (kbd "C-c C-n") #'ocaml-eglot-phrase-next)
    ocaml-eglot-keymap)
  "Keymap for OCaml-eglot minor mode.")


;;;###autoload
(define-minor-mode ocaml-eglot
  "Minor mode for interacting with `ocaml-lsp-server' using `eglot' as a client.
`ocaml-eglot' provides standard implementations of the various custom-requests
 exposed by `ocaml-lsp-server'."
  :lighter " ocaml-eglot"
  :keymap ocaml-eglot-map
  :group 'ocaml-eglot
  (add-hook 'find-file-hook #'ocaml-eglot--file-hook))

(provide 'ocaml-eglot)
;;; ocaml-eglot.el ends here

