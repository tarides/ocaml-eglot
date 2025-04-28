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
;; Package-Version: 1.1
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
(require 'flycheck nil 'noerror)
(require 'xref)
(require 'cl-lib)
(require 'ocaml-eglot-util)
(require 'ocaml-eglot-req)
(require 'ocaml-eglot-type-enclosing)
(require 'eglot)

(declare-function flycheck-next-error "ext:flycheck")
(declare-function flycheck-previous-error "ext:flycheck")

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

(defcustom ocaml-eglot-syntax-checker 'flymake
  "Defines the syntax checker to use."
  :group 'ocaml-eglot
  :type `(choice
          (const :tag "Use Flymake" flymake)
          ,@(when (and (featurep 'flycheck)
                       (featurep 'flycheck-eglot))
              '((const :tag "Use Flycheck" flycheck)))))

(defcustom ocaml-eglot-objinfo-flags
  (list "-shape" "-index" "-decls" "-uid-deps")
  "Flags passed to `ocamlobjinfo'."
  :type '(set
          (const "-shape")
          (const "-index")
          (const "-decls")
          (const "-uid-deps")))

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

;; Custom extension

(defcustom ocaml-eglot-client-capabilities
  (list "jumpToNextHole")
  "List of custom client commands."
  :type '(set (const "jumpToNextHole")))

;;; Features

;; Jump to errors

(defun ocaml-eglot--invalid-syntax-checker ()
  "Error when trying to trigger an invalid syntax checker."
  (error "Unknown syntax checker: %s" ocaml-eglot-syntax-checker))

(defun ocaml-eglot-error-next ()
  "Jump to the next error."
  (interactive)
  (pcase ocaml-eglot-syntax-checker
    ('flymake (call-interactively #'flymake-goto-next-error))
    ('flycheck (call-interactively #'flycheck-next-error))
    (_ (ocaml-eglot--invalid-syntax-checker))))

(defun ocaml-eglot-error-prev ()
  "Jump to the previous error."
  (interactive)
  (pcase ocaml-eglot-syntax-checker
    ('flymake (call-interactively #'flymake-goto-prev-error))
    ('flycheck (call-interactively #'flycheck-previous-error))
    (_ (ocaml-eglot--invalid-syntax-checker))))

;; Jump to definition

(defun ocaml-eglot--find (strategy query)
  "Find the definition using QUERY and jump to it using STRATEGY."
  (let* ((query-result (funcall query))
         (result (ocaml-eglot-util--vec-first-or-nil query-result)))
    (if result
        (let* ((uri (cl-getf result :uri))
               (range (cl-getf result :range))
               (file (ocaml-eglot-util--uri-to-path uri)))
          (ocaml-eglot-util--visit-file strategy (buffer-file-name) file range))
      (eglot--error "Not in environment"))))

(defun ocaml-eglot--find-definition (strategy)
  "Find the definition at point and jump to it using STRATEGY."
  (ocaml-eglot--find strategy #'ocaml-eglot-req--definition))

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

(defun ocaml-eglot--identifier-query (identifier kind)
  "Build the locate IDENTIFIER query based on  KIND."
  (lambda () (let* ((result (ocaml-eglot-req--locate-ident identifier kind))
                    (result-value (ocaml-eglot-util--merlin-call-result result))
                    (loc (ocaml-eglot-util--merlin-location-to-lsp result-value)))
               (make-vector 1 loc))))

(defun ocaml-eglot--find-identifier-definition (identifier strategy)
  "Find the definition of IDENTIFIER jump to it using STRATEGY."
  (ocaml-eglot--find
   strategy (ocaml-eglot--identifier-query identifier 'implementation)))

(defun ocaml-eglot-find-identifier-definition (identifier)
  "Find the definition of the given IDENTIFIER."
  (interactive "s> ")
  (ocaml-eglot--find-identifier-definition
   identifier ocaml-eglot-open-window-strategy))

(defun ocaml-eglot-find-identifier-definition-in-new-window (identifier)
  "Find the definition of the IDENTIFIER and show it in a new window."
  (interactive "s> ")
  (ocaml-eglot--find-identifier-definition identifier 'new))

(defun ocaml-eglot-find-identifier-definition-in-current-window (identifier)
  "Find the definition of the IDENTIFIER and show it in the current window."
  (interactive "s> ")
  (ocaml-eglot--find-identifier-definition identifier 'current))

;; Jump to declaration

(defun ocaml-eglot--find-declaration (strategy)
  "Find the declaration of the identifier at point and jump to it using STRATEGY."
  (ocaml-eglot--find strategy #'ocaml-eglot-req--declaration))

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

(defun ocaml-eglot--find-identifier-declaration (identifier strategy)
  "Find the declaration of IDENTIFIER jump to it using STRATEGY."
  (ocaml-eglot--find
   strategy (ocaml-eglot--identifier-query identifier 'interface)))

(defun ocaml-eglot-find-identifier-declaration (identifier)
  "Find the declaration of the given IDENTIFIER."
  (interactive "s> ")
  (ocaml-eglot--find-identifier-declaration
   identifier ocaml-eglot-open-window-strategy))

(defun ocaml-eglot-find-identifier-declaration-in-new-window (identifier)
  "Find the declaration of the given IDENTIFIER and show it in a new window."
  (interactive "s> ")
  (ocaml-eglot--find-identifier-declaration identifier 'new))

(defun ocaml-eglot-find-identifier-declaration-in-current-window (identifier)
  "Find the declaration of the given IDENTIFIER and show it in the current window."
  (interactive "s> ")
  (ocaml-eglot--find-identifier-declaration identifier 'current))

;; Jump type declaration of expression

(defun ocaml-eglot--find-type-definition (strategy)
  "Find the definition of the type of the expression at point using STRATEGY."
  (let* ((query-result (ocaml-eglot-req--type-definition))
         (result (ocaml-eglot-util--vec-first-or-nil query-result)))
    (if result
        (let* ((uri (cl-getf result :uri))
               (range (cl-getf result :range))
               (file (ocaml-eglot-util--uri-to-path uri)))
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
  (ocaml-eglot-req--server-capable-or-lose :experimental :ocamllsp :handleInferIntf)
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

(defun ocaml-eglot-alternate-file (&optional in-other-window)
  "Visit the alternative file (ml to mli and vice versa).

If optional IN-OTHER-WINDOW is non-nil, find the file in another window."
  (interactive "P")
  ;; We don't rely on `tuareg-find-alternate-file‘ because the
  ;; interface generation relies on `ocamlmerlin’.
  (ocaml-eglot-req--server-capable-or-lose :experimental :ocamllsp :handleSwitchImplIntf)
  (when-let* ((current-uri (ocaml-eglot-util--current-uri))
              (uri (ocaml-eglot--find-alternate-file current-uri))
              (file (ocaml-eglot-util--uri-to-path uri)))
    (if in-other-window
        (find-file-other-window file)
      (find-file file))))

;; Hook when visiting new interface file

(defun ocaml-eglot--file-hook ()
  "Hook to try to generate interface on visiting new files.."
  (when (and
         (ocaml-eglot-util--on-interface)
         (= (buffer-size) 0))
    (ocaml-eglot-infer-interface t )))

;; Holes

(defun ocaml-eglot--first-hole-aux (holes pos comparison)
  "Return the first hole of the list HOLES since a POS using COMPARISON."
  (when (and holes (not (equal [] holes)))
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
  (if (ocaml-eglot-req--server-capable :experimental :ocamllsp :handleJumpToTypedHole)
      (let ((range `(:start ,start :end ,end)))
        (ocaml-eglot-req--hole start 'next range))
    (let* ((holes (ocaml-eglot-req--holes))
           (hole (ocaml-eglot--first-hole-at holes start '>=)))
      (when hole
        (let ((hole-start (cl-getf hole :start))
              (hole-end (cl-getf hole :end)))
          (when (and (>= (ocaml-eglot-util--compare-position hole-start start) 0)
                     (<= (ocaml-eglot-util--compare-position hole-end end) 0))
            hole))))))

(defun ocaml-eglot--first-hole-in (start end)
  "Jump to the first hole in a given range denoted by START and END."
  (when-let* ((hole (ocaml-eglot--get-first-hole-in start end))
              (hole-start (cl-getf hole :start)))
    (ocaml-eglot-util--jump-to hole-start)))

(defun ocaml-eglot--hole-prev ()
  "Jump to the previous hole."
  (ocaml-eglot-req--server-capable-or-lose :experimental :ocamllsp :handleTypedHoles)
  (let* ((current-pos (eglot--pos-to-lsp-position))
         (holes (reverse (ocaml-eglot-req--holes)))
         (hole (ocaml-eglot--first-hole-at holes current-pos '<)))
    (when hole (ocaml-eglot-util--jump-to-range hole))))

(defun ocaml-eglot--hole-next ()
  "Jump to the next hole."
  (ocaml-eglot-req--server-capable-or-lose :experimental :ocamllsp :handleTypedHoles)
  (let* ((current-pos (eglot--pos-to-lsp-position))
         (holes (ocaml-eglot-req--holes))
         (hole (ocaml-eglot--first-hole-at holes current-pos '>)))
    (when hole (ocaml-eglot-util--jump-to-range hole))))

(defun ocaml-eglot--hole-jump (direction)
  "Jump to the following hole (by DIRECTION)."
  (if (ocaml-eglot-req--server-capable :experimental :ocamllsp :handleJumpToTypedHole)
      (let* ((current-pos (eglot--pos-to-lsp-position))
             (range (ocaml-eglot-util--current-range-or-nil))
             (hole (ocaml-eglot-req--hole current-pos direction range)))
        (when hole (ocaml-eglot-util--jump-to-range hole)))
    (pcase direction
      ('prev (ocaml-eglot--hole-prev))
      (_ (ocaml-eglot--hole-next)))))

(defun ocaml-eglot-hole-prev ()
  "Jump to the previous hole."
  (interactive)
  (ocaml-eglot--hole-jump 'prev))

(defun ocaml-eglot-hole-next ()
  "Jump to the next hole."
  (interactive)
  (ocaml-eglot--hole-jump 'next))

;; Jump to source elements

(defun ocaml-eglot-jump ()
  "Jumps to the the closest fun/let/match/module/module-type/match-case."
  (interactive)
  (ocaml-eglot-req--server-capable-or-lose :experimental :ocamllsp :handleJump)
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
The universal prefix argument can be used to change the maximum number
of result (LIMIT).  KEY define the current value to be selected."
  (ocaml-eglot-req--server-capable-or-lose :experimental :ocamllsp :handleTypeSearch)
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
The universal prefix argument can be used to change the maximum number
of results (LIMIT)."
  (interactive "sSearch query: \np")
  (ocaml-eglot-req--server-capable-or-lose :experimental :ocamllsp :handleTypeSearch)
  (let* ((start (eglot--pos-to-lsp-position))
         (chosen (ocaml-eglot--search query limit :constructible))
         (result (concat "(" chosen ")"))
         (end (ocaml-eglot-util--position-increase-char start result)))
    (when (region-active-p)
      (delete-region (region-beginning) (region-end)))
    (insert result)
    (ocaml-eglot--first-hole-in start end)))

(defun ocaml-eglot--search-def-or-decl (callback query &optional limit)
  "Search a definition or a declaration using a QUERY (type or polarity).
The universal prefix argument can be used to change the maximum number
of results (LIMIT).  CALLBACK is used to define the jump strategy."
  (ocaml-eglot-req--server-capable-or-lose :experimental :ocamllsp :handleTypeSearch)
  (let ((result (ocaml-eglot--search query limit :name)))
    (funcall callback result)))

(defun ocaml-eglot-search-definition (query &optional limit)
  "Search a definition using a QUERY (type or polarity).
The universal prefix argument can be used to change the maximum number
of results (LIMIT)."
  (interactive "sSearch query: \np")
  (ocaml-eglot--search-def-or-decl
   #'ocaml-eglot-find-identifier-definition
   query
   limit))

(defun ocaml-eglot-search-definition-in-current-window (query &optional limit)
  "Search a definition using a QUERY (type or polarity) in the current window.
The universal prefix argument can be used to change the maximum number
of results (LIMIT)."
  (interactive "sSearch query: \np")
  (ocaml-eglot--search-def-or-decl
   #'ocaml-eglot-find-identifier-definition-in-current-window
   query
   limit))

(defun ocaml-eglot-search-definition-in-new-window (query &optional limit)
  "Search a definition using a QUERY (type or polarity) in a new window.
The universal prefix argument can be used to change the maximum number
of results (LIMIT)."
  (interactive "sSearch query: \np")
  (ocaml-eglot--search-def-or-decl
   #'ocaml-eglot-find-identifier-definition-in-new-window
   query
   limit))

(defun ocaml-eglot-search-declaration (query &optional limit)
  "Search a declaration using a QUERY (type or polarity).
The universal prefix argument can be used to change the maximum number
of results (LIMIT)."
  (interactive "sSearch query: \np")
  (ocaml-eglot--search-def-or-decl
   #'ocaml-eglot-find-identifier-declaration
   query
   limit))

(defun ocaml-eglot-search-declaration-in-current-window (query &optional limit)
  "Search a declaration using a QUERY (type or polarity) in the current window.
The universal prefix argument can be used to change the maximum number
of results (LIMIT)."
  (interactive "sSearch query: \np")
  (ocaml-eglot--search-def-or-decl
   #'ocaml-eglot-find-identifier-declaration-in-current-window
   query
   limit))

(defun ocaml-eglot-search-declaration-in-new-window (query &optional limit)
  "Search a declaration using a QUERY (type or polarity) in a new window.
The universal prefix argument can be used to change the maximum number
of results (LIMIT)."
  (interactive "sSearch query: \np")
  (ocaml-eglot--search-def-or-decl
   #'ocaml-eglot-find-identifier-declaration-in-new-window
   query
   limit))

;; Construct

(defun ocaml-eglot--construct-local-values (with-local-value)
  "Constructs WITH-LOCAL-VALUE parameter for the `construct’ query."
  (if (or with-local-value ocaml-eglot-construct-with-local-values)
      "local"
    "none"))

(defun ocaml-eglot-construct (&optional arg)
  "Construct over the current hole.
It use the ARG to use local values or not."
  (interactive "P")
  (ocaml-eglot-req--server-capable-or-lose :experimental :ocamllsp :handleConstruct)
  (let* ((current-range (ocaml-eglot-util--current-range))
         (start (cl-getf current-range :start))
         (end (cl-getf current-range :end))
         (hole (ocaml-eglot--get-first-hole-in start end)))
    (if (not hole)
        (eglot--error "Not a hole")
      (let* ((with-local-value (ocaml-eglot--construct-local-values arg))
             (hole-start (cl-getf hole :start))
             (result (ocaml-eglot-req--construct hole-start 1 with-local-value))
             (range (cl-getf result :position))
             (suggestions (append (cl-getf result :result) nil)))
        (when (equal suggestions [])
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
              (insert-construct-choice choice))))))))

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
  (if (region-active-p)
      (ocaml-eglot-req--destruct (region-beginning) (region-end))
    (ocaml-eglot-req--destruct (point) (point))))

;; Occurences

(defun ocaml-eglot-occurences ()
  "Find all occurrences of the identifier under the cursor."
  (interactive)
  (call-interactively #'xref-find-references))

(defun ocaml-eglot-rename ()
  "Rename the symbol at point."
  (interactive)
  (call-interactively #'eglot-rename))

;;; Custom command handler

(defun ocaml-eglot--command-next-hole (arguments)
  "Perform the command `ocaml.next-hole' using ARGUMENTS."
  (let* ((range (cl-getf (aref arguments 0) :inRange))
         (start (cl-getf range :start))
         (end (cl-getf range :end)))
    (ocaml-eglot--first-hole-in start end)))

;;; Overriding

;; We use `ocaml-eglot-client-capabilities' to list all the
;; capabilities supported by the client (the editor) and provision
;; them to the server as supported capabilities.

(cl-defmethod eglot-client-capabilities :around (_)
  "Add client capabilities to Eglot for OCaml LSP server."
  (let* ((capabilities (copy-tree (cl-call-next-method)))
         (experimental-capabilities (cl-getf capabilities :experimental))
         (previous (or experimental-capabilities eglot--{}))
         (commands (append (apply #'vector ocaml-eglot-client-capabilities) nil)))
    (dolist (key commands)
      (puthash key t previous))
    (setq capabilities (plist-put capabilities :experimental previous))))

;; A command can be executed by the server or by the client. The
;; following code analyses the command. If it's a command which must
;; be implemented by the editor (such as `ocaml.next-hole', we execute
;; it, otherwise we leave it to the previous implementation.

(when (fboundp 'eglot-execute)
  (cl-defmethod eglot-execute :around (_ action)
    "Custom handler for performing client commands."
    (pcase (cl-getf action :command)
      ("ocaml.next-hole" (ocaml-eglot--command-next-hole
                          (cl-getf action :arguments)))
      (_ (cl-call-next-method)))))

;; The way `Eglot' handles commands changed between version `29.1' and
;; `30.x', hence the replicate between `eglot-execute' (>= 30) and
;; `eglot-execute-command' (< 30).

(when (fboundp 'eglot-execute-command)
  (cl-defmethod eglot-execute-command :around (_ command arguments)
    "Custom handler for performing client commands (legacy)."
    (pcase command
      ("ocaml.next-hole" (ocaml-eglot--command-next-hole arguments))
      (_ (cl-call-next-method)))))

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
OCaml Eglot provides standard implementations of the various custom-requests
 exposed by `ocaml-lsp-server'."
  :lighter " OCaml-eglot"
  :keymap ocaml-eglot-map
  :group 'ocaml-eglot
  (add-hook 'find-file-hook #'ocaml-eglot--file-hook))

;;; Ocamlobjinfo mode

;; Allows known compilation artefacts to be displayed via the
;; `ocamlobjinfo' binary

(define-derived-mode ocaml-eglot-objinfo-mode special-mode "OCaml-obj-info"
  "Mode for displaying ocamlobjinfo output."
  (setq buffer-read-only t
        buffer-offer-save nil
        buffer-file-name nil
        auto-save-default nil
        make-backup-files nil
        create-lockfiles nil)
  (define-key ocaml-eglot-objinfo-mode-map (kbd "q") #'quit-window))

;;;###autoload
(defun ocaml-eglot-objinfo-handler ()
  "Display the result of `ocamlobjinfo` instead of file contents."
  (when (and buffer-file-name
             (ocaml-eglot-util--is-artifact buffer-file-name))
    (let ((file buffer-file-name)
          (inhibit-read-only t))
      (setq buffer-read-only nil)
      (erase-buffer)
      (if (executable-find "ocamlobjinfo")
          (let ((args (append ocaml-eglot-objinfo-flags (list file))))
            (apply #'call-process "ocamlobjinfo" nil t nil args))
        (insert "`ocamlobjinfo' not found in PATH"))
      (goto-char (point-min))
      (ocaml-eglot-objinfo-mode))))

;;;###autoload
(add-hook 'find-file-hook #'ocaml-eglot-objinfo-handler)

(provide 'ocaml-eglot)
;;; ocaml-eglot.el ends here
