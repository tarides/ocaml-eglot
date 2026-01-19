;;; ocaml-eglot-xref.el --- Xref backend for OCaml   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2025  The OCaml-eglot Project Contributors
;; Licensed under the MIT license.

;; Author: Xavier Van de Woestyne <xaviervdw@gmail.com>
;;         Spencer Baugh
;; Created: 10 June 2025
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; An Xref backend for OCaml.

;;; Code:

(require 'cl-lib)
(require 'xref)
(require 'eglot)
(require 'ocaml-eglot-util)
(require 'ocaml-eglot-req)

(defcustom ocaml-eglot-locate-preference 'ml
  "Determine whether locate should in priority look in ml or mli files."
  :group 'ocaml-eglot
  :type '(choice (const :tag "Look at implementation" ml)
                 (const :tag "Look at interfaces" mli)))

(defun ocaml-eglot-xref-backend ()
  "OCaml-eglot backend for Xref."
  'ocaml-eglot-xref)

(defun ocaml-eglot-xref--call-occurrences (pt)
  "Call `:textDocument/references' for a given PT."
  (ocaml-eglot-req--send
   :textDocument/references
   (append
    (ocaml-eglot-req--TextDocumentPositionParamsWithPos
     (eglot--pos-to-lsp-position pt))
    (list :context (list :includeDeclaration t)))))

(cl-defmethod xref-backend-references ((_backend (eql ocaml-eglot-xref)) symbol)
  "An `xref-backend-references' for SYMBOL for OCaml-eglot."
  (xref-backend-references 'eglot symbol))

(cl-defmethod xref-backend-definitions ((_backend (eql ocaml-eglot-xref)) symbol)
  "Extension of `xref-backend-definitions' for SYMBOL."
  (condition-case err
      (if-let* ((pt (get-text-property 0 'eglot--lsp-workspaceSymbol symbol)))
          ;; SYMBOL is from `xref-backend-identifier-at-point',
          ;; since if it was read from the minibuffer its text
          ;; properties would have been stripped
          ;; (see `minibuffer-allow-text-properties').  Just pass
          ;; position and Merlin will figure out everything from that.
          (eglot--lsp-xrefs-for-method
           (if (eq ocaml-eglot-locate-preference 'mli)
               :textDocument/declaration
             :textDocument/definition))
        ;; The LSP doesn't support jumping to definition of an arbitrary identifier,
        ;; so we have to fallback on ocamllsp/locate.
        (if-let* ((locate-result
                   (ocaml-eglot-req--send
                    :ocamllsp/locate
                    (append (ocaml-eglot-req--TextDocumentPositionParamsWithPos
                             (eglot--pos-to-lsp-position (point)))
                            (list :kind (if (eq ocaml-eglot-locate-preference 'mli)
                                            "declaration" "definition")
                                  :prefix (string-remove-suffix "." symbol)))))
                  (result (ocaml-eglot-util--vec-first-or-nil locate-result)))
            (list (eglot--xref-make-match symbol
                                          (cl-getf result :uri)
                                          (cl-getf result :range)))))
    (jsonrpc-error (ocaml-eglot-req--locate-fallback err))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql ocaml-eglot-xref)))
  "Return a list of symbols for completion."
  ;; TODO: xref-backend-identifier-completion-table
  ;; missing `merlin-cap-table'
  nil)

(defconst ocaml-eglot-xref--operator-regexp
  (eval-when-compile
    (let* ((core-operator-char
            `(or "$" "&" "*" "+" "-" "/" "=" ">" "@" "^" "|"))
           (operator-char `(or "~" "!" "?" ,core-operator-char "%" "<" ":" "."))
           (prefix-symbol `(or (seq "!" (* ,operator-char))
                               (seq (or "?" "~") (+ ,operator-char))))
           (infix-symbol `(or (seq (or ,core-operator-char "%" "<")
                                   (* ,operator-char))
                              (seq "#" (+ ,operator-char))))
           (infix-op `(or ,infix-symbol
                          ":="
                          ;; Already handled as part of `infix-symbol':
                          ;; "*" "+" "-" "-." "=" "!=" "<" ">" "||" "&" "&&"
                          ;; Treated as normal symbols:
                          ;; "or" "mod" "land" "lor" "lxor" "lsl" "lsr" "asr"
                          ))
           (operator-name `(or ,prefix-symbol ,infix-op)))
      (rx-to-string operator-name t))))

(defconst ocaml-eglot-xref--binding-operator-regexp
  (eval-when-compile
    (let* ((core-operator-char
            `(or "$" "&" "*" "+" "-" "/" "=" ">" "@" "^" "|"))
           (dot-operator-char
            `(or "!" "?" ,core-operator-char "%" ":"))
           (binding-suffix
            `(seq (or ,core-operator-char "<") (* ,dot-operator-char)))
           (binding-operator
            `(seq symbol-start (or "let" "and") ,binding-suffix)))
      (rx-to-string binding-operator t))))

(defconst ocaml-eglot-xref--identifier-regexp
  (rx symbol-start (in "A-Za-z_") (* (in "A-Za-z0-9_'"))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql ocaml-eglot-xref)))
  "Extension of `xref-backend-identifier-at-point'."
  (let ((symbol
         (cond
          ;; binding operator starting at point
          ((looking-at ocaml-eglot-xref--binding-operator-regexp)
           (match-string 0))
          ;; ... before point
          ((and (save-excursion
                  (skip-chars-backward "letand$&*+/=<>@^|!?%:-")
                  (looking-at ocaml-eglot-xref--binding-operator-regexp))
                (<= (point) (match-end 0)))
           (match-string 0))
          ;; ordinary name starting at point
          ((looking-at ocaml-eglot-xref--identifier-regexp)
           (save-excursion
             ;; include Module.Context. before point
             (skip-chars-backward "A-Za-z0-9_'.")
             (buffer-substring (point) (match-end 0))))
          ;; operator starting at or before point
          ((and (save-excursion
                  (skip-chars-backward "$&*+/=<>@^|!?%:.~#-")
                  (looking-at ocaml-eglot-xref--operator-regexp))
                (<= (point) (match-end 0)))
           (match-string 0))
          ;; ordinary name starting before point
          ((and (save-excursion
                  (skip-chars-backward "A-Za-z0-9_'")
                  (looking-at ocaml-eglot-xref--identifier-regexp))
                (<= (point) (match-end 0)))
           (save-excursion
             ;; include Module.Context. before point
             (skip-chars-backward "A-Za-z0-9_'.")
             (buffer-substring (point) (match-end 0)))))))
    ;; Return a string with the buffer position in a property, in case
    ;; point changes before the string is used by one of the methods above.
    (and symbol (propertize symbol 'eglot--lsp-workspaceSymbol (point)))))

(provide 'ocaml-eglot-xref)
;;; ocaml-eglot-xref.el ends here
