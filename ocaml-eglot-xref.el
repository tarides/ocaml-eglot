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

(cl-defstruct ocaml-eglot-xref-location
  "A location suitable for passing to xref.
Note that while pos contains a column, it's a byte offset rather
than a character offset, so we can't use `xref-make-file-location'."
  (file nil :type string)
  (line nil :type number)
  (pos nil :type plist))

(cl-defmethod xref-location-group ((l ocaml-eglot-xref-location))
  "Implementation of `xref-make-file-location' for L."
  (ocaml-eglot-xref-location-file l))

(cl-defmethod xref-location-line ((l ocaml-eglot-xref-location))
  "Implementation of `xref-make-line-location' for L."
  (ocaml-eglot-xref-location-line l))

(cl-defmethod xref-location-marker ((l ocaml-eglot-xref-location))
  "Implementation of `xref-make-file-marker' for L."
  ;; Mostly copied from the implementations for `xref-make-file-location'
  ;; and `xref-make-buffer-location'.
  (let* ((buffer
          (let ((find-file-suppress-same-file-warnings t))
            (find-file-noselect (ocaml-eglot-xref-location-file l))))
         (pos (with-current-buffer buffer
                (eglot--lsp-position-to-point
                 (ocaml-eglot-xref-location-pos l)))))
    (move-marker (make-marker) pos buffer)))

(defun ocaml-eglot-xref--call-occurrences (pt)
  "Call `:textDocument/references' for a given PT."
  (ocaml-eglot-req--send
   :textDocument/references
   (append
    (ocaml-eglot-req--TextDocumentPositionParamsWithPos
     (eglot--pos-to-lsp-position pt))
    (list :context (list :includeDeclaration t)))))

(defun ocaml-eglot-xref--locate (pt)
  "Perform conditionnaly find definition or find declaration for a given PT."
  (ocaml-eglot-util--vec-first-or-nil
   (ocaml-eglot-req--send
    (if (eq ocaml-eglot-locate-preference 'mli)
        :textDocument/declaration
      :textDocument/definition)
    (ocaml-eglot-req--TextDocumentPositionParamsWithPos
     (eglot--pos-to-lsp-position pt))
    :fallback #'ocaml-eglot-req--locate-fallback)))

(defun ocaml-eglot-xref--call-locate (symbol)
  "Locate an identifier based on SYMBOL used for xref."
  (if-let* ((pt (get-text-property 0 'ocaml-eglot-xref-point symbol)))
      ;; SYMBOL is from `xref-backend-identifier-at-point',
      ;; since if it was read from the minibuffer its text
      ;; properties would have been stripped
      ;; (see `minibuffer-allow-text-properties').  Just pass
      ;; position and Merlin will figure out everything from that.
      (let ((result (ocaml-eglot-xref--locate pt)))
        (list :file (ocaml-eglot-util--uri-to-path (cl-getf result :uri))
              :pos (cl-getf (cl-getf result :range) :start)))
    ;; The LSP doesn't support jumping to definition of an arbitrary identifier,
    ;; so we have to fallback on ocamllsp/locate.
    (let* ((locate-result
            (ocaml-eglot-req--send
             :ocamllsp/locate
             (append (ocaml-eglot-req--TextDocumentPositionParamsWithPos
                      (eglot--pos-to-lsp-position (point)))
                     (list :kind (if (eq ocaml-eglot-locate-preference 'mli)
                                     "declaration" "definition")
                           :prefix (string-remove-suffix "." symbol)))))
           (pos (ocaml-eglot-util--vec-first-or-nil locate-result)))
      (when pos
        (list :file (ocaml-eglot-util--uri-to-path (cl-getf pos :uri))
              :pos (cl-getf (cl-getf pos :range) :start))))))

(defun ocaml-eglot-xref--occurrences (symbol)
  "Compute occurrences for the given SYMBOL."
  (let ((pt (get-text-property 0 'ocaml-eglot-xref-point symbol)))
    (cl-assert pt nil "OCaml-eglot xref-find-references cannot be used by explicitly typing in a symbol %s" symbol)
    (let ((result (ocaml-eglot-xref--call-occurrences pt)))
      ;; Change the vector into a list
      (append result nil))))

(defun ocaml-eglot-xref--buffer (file)
  "Compute a buffer in term of FILE based on occurrences results."
  (if (equal file "/*buffer*")
      ;; occurrences returns "/*buffer*" as the filename for occurrences
      ;; in the current buffer when we don't pass the -filename argument.
      (current-buffer)
    ;; Look for an existing buffer with this file, but don't open it
    ;; if it's not already open.
    (get-file-buffer file)))


(defun ocaml-eglot-xref--make-location-in-file (file pos)
  "Turn FILE and POS into an `xref-item'.
Requires that the current buffer be the buffer of FILE."
  ;; If we didn't send a filename with the merlin call, we get back "*buffer*"
  ;; as the filename for locate, "/*buffer*" for occurrences.
  (if (member file '("*buffer*" "*/buffer*"))
      ;; We have to remember the current buffer, rather than reading
      ;; it from the filesystem again later.
      (xref-make-buffer-location (current-buffer)
                                 (eglot--lsp-position-to-point pos))
    (make-ocaml-eglot-xref-location :file file
                                    :line (1+ (cl-getf pos :line))
                                    :pos pos)))

(defun ocaml-eglot-xref--push-marker (symbol buffer start end loc)
  "Push computed marker for SYMBOL by START and END with LOC on given BUFFER."
  (if buffer
      ;; We have the file open, or this is just a reference found in
      ;; the current buffer.  We can populate the summary field with real data,
      ;; and decode the byte-based `start' and `end' into the character
      ;; length of the reference.
      (with-current-buffer buffer
        (xref-make-match
         (concat
          (buffer-substring (save-excursion (goto-char start) (pos-bol)) start)
          (propertize (buffer-substring start end) 'face 'xref-match)
          (buffer-substring (save-excursion (goto-char end) (pos-eol)) end))
         loc
         (- end start)))
    ;; We don't have the file open, so we can't make a real summary or decode
    ;; the length.  We'll just use the symbol as the summary instead.
    (xref-make symbol loc)))

(cl-defmethod xref-backend-references ((_backend (eql ocaml-eglot-xref)) symbol)
  "An `xref-backend-references' for SYMBOL for OCaml-eglot."
  (let ((occurrences (ocaml-eglot-xref--occurrences symbol))
        result)
    (dolist (loc occurrences)
      (let* ((file (ocaml-eglot-util--uri-to-path (cl-getf loc :uri)))
             (start-pos (cl-getf (cl-getf loc :range) :start))
             (end-pos (cl-getf (cl-getf loc :range) :end))
             (buffer (ocaml-eglot-xref--buffer file))
             (location (ocaml-eglot-xref--make-location-in-file file start-pos)))
        (push (ocaml-eglot-xref--push-marker
               symbol
               buffer
               (eglot--lsp-position-to-point start-pos)
               (eglot--lsp-position-to-point end-pos)
               location) result)))
    (reverse result)))

(cl-defmethod xref-backend-definitions ((_backend (eql ocaml-eglot-xref)) symbol)
  "Extension of `xref-backend-definitions' for SYMBOL."
  (let ((result (ocaml-eglot-xref--call-locate symbol)))
    (unless result (error "Not found.  (Check *Messages* for potential errors)"))
    ;; In this case, an error is returned.
    (if (stringp result) (user-error "%s" result))
    (list
     (xref-make
      symbol
      (ocaml-eglot-xref--make-location-in-file (cl-getf result :file)
                                               (cl-getf result :pos))))))


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
    (and symbol (propertize symbol 'ocaml-eglot-xref-point (point)))))

(provide 'ocaml-eglot-xref)
;;; ocaml-eglot-xref.el ends here
