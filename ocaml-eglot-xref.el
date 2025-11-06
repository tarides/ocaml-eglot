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
Note that while merlin-pos contains a column, it's a byte offset rather
than a character offset, so we can't use `xref-make-file-location'."
  (file nil :type string)
  (line nil :type number)
  (merlin-pos nil :type plist))

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
                (ocaml-eglot-util--pos-to-point
                 (ocaml-eglot-xref-location-merlin-pos l)))))
    (move-marker (make-marker) pos buffer)))

(defun ocaml-eglot-xref--call-occurences (pt)
  "Call merlin-occurences for given PT."
  (let ((argv (vector "-scope" "project"
                      "-identifier-at" (ocaml-eglot-util-point-as-arg pt))))
    (ocaml-eglot-req--merlin-call "occurrences" argv)))

(defun ocaml-eglot-xref--call-locate (symbol)
  "Locate an idenfier based on SYMBOL used for xref."
  (let ((argv (if-let* ((pt (get-text-property 0 'ocaml-eglot-xref-point symbol)))
                  ;; SYMBOL is from `xref-backend-identifier-at-point',
                  ;; since if it was read from the minibuffer its text
                  ;; properties would have been stripped
                  ;; (see `minibuffer-allow-text-properties').  Just pass
                  ;; position and Merlin will figure out everything from that.
                  (vector "-position" (ocaml-eglot-util-point-as-arg pt)
                          "-look-for" (symbol-name ocaml-eglot-locate-preference))
                ;; SYMBOL was probably just typed in by the user.  So pass it
                ;; to Merlin, removing a trailing "." in case the user completed
                ;; a module name with `merlin-cap-dot-after-module':
                (vector "-prefix" (string-remove-suffix "." symbol)
                        ;; We don't know if SYMBOL is a module or type or
                        ;; expr, and we shouldn't use -position to guess.
                        ;; See: https://github.com/janestreet/merlin-jst/pull/91
                        "-context" "unknown"
                        "-position" (ocaml-eglot-util-point-as-arg (point))
                        ;; And use `point' to pick the lexical environment to
                        ;; search:
                        "-look-for" (symbol-name ocaml-eglot-locate-preference)))))
    (ocaml-eglot-req--merlin-call "locate" argv)))

(defun ocaml-eglot-xref--occurences (symbol)
  "Compute occurrences for the given SYMBOL."
  (let ((pt (get-text-property 0 'ocaml-eglot-xref-point symbol)))
    (cl-assert pt nil "OCaml-eglot xref-find-references cannot be used by explicitly typing in a symbol %s" symbol)
    (let ((result (ocaml-eglot-xref--call-occurences pt)))
      ;; Change the vector into a list
      (append (ocaml-eglot-util--merlin-call-result result) nil))))

(defun ocaml-eglot-xref--buffer (file)
  "Compute a buffer in term of FILE based on occurences results."
  (if (equal file "/*buffer*")
      ;; occurences returns "/*buffer*" as the filename for occurences
      ;; in the current buffer when we don't pass the -filename argument.
      (current-buffer)
    ;; Look for an existing buffer with this file, but don't open it
    ;; if it's not already open.
    (get-file-buffer file)))


(defun ocaml-eglot-xref--make-location-in-file (file merlin-pos)
  "Turn FILE and MERLIN-POS into an `xref-item'.
Requires that the current buffer be the buffer of FILE."
  ;; If we didn't send a filename with the merlin call, we get back "*buffer*"
  ;; as the filename for locate, "/*buffer*" for occurrences.
  (if (member file '("*buffer*" "*/buffer*"))
      ;; We have to remember the current buffer, rather than reading
      ;; it from the filesystem again later.
      (xref-make-buffer-location (current-buffer)
                                 (ocaml-eglot-util--pos-to-point merlin-pos))
    (make-ocaml-eglot-xref-location :file file
                                    :line (cl-getf merlin-pos :line)
                                    :merlin-pos merlin-pos)))

(defun ocaml-eglot-xref--push-marker (symbol buffer start loc xref-loc)
  "Push computed marker for SYMBOL by START and LOC/XREF-LOC on the given BUFFER."
  (if buffer
      ;; We have the file open, or this is just a reference found in
      ;; the current buffer.  We can populate the summary field with real data,
      ;; and decode the byte-based `start' and `end' into the character
      ;; length of the reference.
      (with-current-buffer buffer
        (let ((start-pos (ocaml-eglot-util--pos-to-point start))
              (end-pos (ocaml-eglot-util--pos-to-point (cl-getf loc :end))))
          (xref-make-match
           (concat
            (buffer-substring
             (save-excursion (goto-char start-pos) (pos-bol))
             start-pos)
            (propertize (buffer-substring start-pos end-pos) 'face 'xref-match)
            (buffer-substring
             (save-excursion (goto-char end-pos) (pos-eol))
             end-pos))
           xref-loc
           (- end-pos start-pos))))
    ;; We don't have the file open, so we can't make a real summary or decode
    ;; the length.  We'll just use the symbol as the summary instead.
    (xref-make symbol xref-loc)))

(cl-defmethod xref-backend-references ((_backend (eql ocaml-eglot-xref)) symbol)
  "An `xref-backend-references' for SYMBOL for OCaml-eglot."
  (let ((occurences (ocaml-eglot-xref--occurences symbol))
        result)
    (dolist (loc occurences)
      (let* ((file (cl-getf loc :file))
             (start-pos (cl-getf loc :start))
             (buffer (ocaml-eglot-xref--buffer file))
             (location (ocaml-eglot-xref--make-location-in-file file start-pos)))
        (push (ocaml-eglot-xref--push-marker
               symbol buffer start-pos loc location) result)))
    (reverse result)))

(cl-defmethod xref-backend-definitions ((_backend (eql ocaml-eglot-xref)) symbol)
  "Extension of `xref-backend-definitions' for SYMBOL."
  (let* ((result (ocaml-eglot-xref--call-locate symbol))
         (loc (ocaml-eglot-util--merlin-call-result result)))
    (unless loc (error "Not found.  (Check *Messages* for potential errors)"))
    ;; In this case, an error is returned.
    (if (stringp loc) (user-error "%s" loc))
    (list
     (xref-make
      symbol
      (ocaml-eglot-xref--make-location-in-file (cl-getf loc :file)
                                               (cl-getf loc :pos))))))


(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql ocaml-eglot-xref)))
  "Returns a list of symbols for completion."
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
