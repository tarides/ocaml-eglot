;;; ocaml-eglot-completion.el --- Completion at point for OCaml   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2015-2026  The OCaml-eglot Project Contributors
;; Licensed under the MIT license.

;; Author: Simon Castellan <simon.castellan(_)iuwt.fr>
;;         Frédéric Bour <frederic.bour(_)lakaban.net>
;;         Thomas Refis <thomas.refis(_)gmail.com>
;;         Spencer Baugh <sbaugh(_)janestreet.com>
;;         Sean Whitton <spwhitton(_)spwhitton.name>
;; Created: 15 May 2015
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A `completion-at-point' function for OCaml, using ocaml-lsp's
;; `ocamllsp/completePrefixAtPos' custom request.  Compared with Eglot's
;; generic `eglot-completion-at-point', this completes one component of a
;; Dotted.Module.path at a time, which works well with the
;; partial-completion style, and caches completions between calls.
;;
;; Derived from merlin-cap.el from the Merlin project.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'ocaml-eglot-req)

;; Internal variables

(defvar-local ocaml-eglot-completion--cache nil
  "An alist mapping contexts to lists of completions.

This is used to cache completions; it is indexed by the part of
the completion string up to and including its last \".\".

The keys are strings like \"List.\" or the empty string.
The values are lists of completion strings.")

(defvar-local ocaml-eglot-completion--cache-position nil
  "The position for which `ocaml-eglot-completion--cache' is valid.")

(defcustom ocaml-eglot-completion-types t
  "If non-nil, show the types of identifiers during completion."
  :type 'boolean
  :group 'ocaml-eglot)

;; Internal functions

(defun ocaml-eglot-completion--exit-function (string _state)
  "Print a message for completion STRING."
  (let ((ret (ocaml-eglot-completion--annotate string)))
    (if ret (message "%s%s" string ret))))

(defun ocaml-eglot-completion--annotate (candidate)
  "Retrieve the annotation for candidate CANDIDATE."
  ;; Fancy completion styles (like partial-completion) can concatenate multiple
  ;; candidates together into a single candidate (e.g. "List." and "map"
  ;; concatenated into "List.map"), and each component of that will have its
  ;; own annotation text property.  We want to print the type of the overall
  ;; expression, so grab the annotation from the end.
  (and (stringp candidate)
       (not (string-empty-p candidate))
       (get-text-property (1- (length candidate))
                          'ocaml-eglot-completion--annotation candidate)))

(defun ocaml-eglot-completion--complete-prefix (position prefix)
  "Request ocaml-lsp completions that could come after PREFIX at POSITION.

POSITION is some position in the current buffer.  It doesn't need
to actually have PREFIX before it.

Return the result of the ocamllsp/completePrefixAtPos request."
  (let* (;; `throw-on-input' is non-nil when we're inside a `while-no-input'.
         (while-no-input-tag throw-on-input)
         ;; TODO: Consider sending these requests asynchronously.
         (result (ocaml-eglot-req--complete-prefix-at-pos
                  position prefix
                  ;; Necessary because `jsonrpc-request' binds
                  ;; `throw-on-input' to nil.
                  :cancel-on-input (and while-no-input-tag t)
                  :cancel-on-input-retval 'ocaml-eglot-completion--canceled)))
    (if (eq result 'ocaml-eglot-completion--canceled)
        (throw while-no-input-tag t)
      result)))

(defcustom ocaml-eglot-completion-dot-after-module 'not-module-name
  "If non-nil, automatically add a dot after completing a module name.

If the symbol `not-module-name', do this everywhere except when
completing the name of a module itself.  For example, don't add a dot
after List in \"(module List)\" or \"include List\" or \"open List\".

Specifically, the completion strings returned for modules have a
\".\" appended to them which is included when accepting a module
as a completion.  For example, when this is non-nil, the List
module is completed as \"List.\" rather than \"List\".

This is convenient when completing a value name like
\"Foo.Bar.baz\", since you don't have to type \".\" after
completing \"Foo\" and \"Bar\".  When completing a module name
like \"(module Foo.Bar)\", this is less convenient, since you
need to remove the trailing dot after completing \"Bar\".  Since
the former is much more common than the latter, this is on by
default.

This also has a non-obvious effect: when module names have a dot
after them, partial completion works substantially better.  For
example, if this is on, `completion-at-point' will complete
\"Li.map\" to \"List.map\".

The reason for this is somewhat hard to explain, but essentially
the partial completion style tries to expand the module path by
expanding the glob \"Li*.\".  That expands to \"List.\" but not
to \"List\" because the latter doesn't include the trailing dot."
  :type '(choice (const :tag "No . after module names" nil)
                 (const :tag "Always . after module names" t)
                 (const :tag ". after module names, except for include/open/module"
                        not-module-name))
  :group 'ocaml-eglot)

(defcustom ocaml-eglot-completion-suppress-ppx-template nil
  "If non-nil, don't show completions generated by ppx_template."
  :type '(choice (const :tag "Show ppx_template completions" nil)
                 (const :tag "Don't show ppx_template completions" t))
  :group 'ocaml-eglot)

(defun ocaml-eglot-completion--ppx-template-name-p (name)
  "Return non-nil if NAME was generated by ppx_template."
  (string-match (rx "__" (or "'" "local" "immediate" "bits" "float" "word"
                             "portable" "stack" "value"))
                name))

(defun ocaml-eglot-completion--process-completion-data (result prefix)
  "Process RESULT from ocamllsp/completePrefixAtPos into a list of completions.
PREFIX should be the prefix used to request this data."
  (let (ret
        ;; In a function application context, ocaml-lsp returns regular
        ;; completions in addition to the function argument labels.
        ;; We may want to ignore the regular completions when there are
        ;; function argument labels, I'm not sure yet.
        (labels
         (pcase (plist-get result :context)
           ('nil nil)
           (`["application" ,application]
            (plist-get application :labels))
           (context
            (error "Unexpected ocamllsp/completePrefixAtPos context: %S" context))))
        (dot-after-module
         (if (eq ocaml-eglot-completion-dot-after-module 'not-module-name)
             ;; It would be nice if Merlin told us whether point is in a
             ;; context which wants to complete a module name, but it doesn't,
             ;; so we hack it instead.
             (let* ((atom (ocaml-eglot-completion--atom-bounds))
                    (completing-module-name
                     ;; % indicates we're writing the name of a module for a ppx
                     (or (eq (char-before (car atom)) ?%)
                         (save-excursion
                           (goto-char (car atom))
                           (skip-chars-backward " ")
                           (let ((word-end (point)))
                             (skip-chars-backward "a-z!")
                             (member (buffer-substring-no-properties (point) word-end)
                                     '("module" "include" "functor" "open" "open!")))))))
               (not completing-module-name))
           ocaml-eglot-completion-dot-after-module)))
    ;; Collect all the normal completion entries
    (seq-doseq (item (plist-get result :entries))
      (let ((name (or (plist-get item :label)
                      (error "Item without :label from ocamllsp/completePrefixAtPos: %S"
                             item)))
            (kind (pcase (plist-get item :kind)
                    (12 "Value") (20 "Variant") (5 "Label") (9 "Module") (8 "Modtype")
                    (2 "MethodCall") (14 "Keyword") (4 "Constructor") (25 "Type")
                    (_ "Value"))))
        (put-text-property 0 (length name) 'ocaml-eglot-completion-kind kind name)
        ;; We could delay some of this work to ocaml-eglot-completion--annotate,
        ;; but it doesn't help much in benchmarks.
        (put-text-property
         0 (length name) 'ocaml-eglot-completion--annotation
         (let ((desc (replace-regexp-in-string "[\n ]+" " "
                                               (or (plist-get item :detail) ""))))
           (cond
            ((equal kind "Module")
             ;; Since we're producing different results based on
             ;; `ocaml-eglot-completion-dot-after-module', and those results go
             ;; into the cache, and we aren't invalidating the cache when that
             ;; variable changes, if the user changes that variable and then
             ;; tries to complete again without moving to a different OCaml
             ;; atom (which would invalidate the cache), they will get
             ;; completions without a dot, pulled from the cache.  Not worth
             ;; fixing IMO.
             (if dot-after-module
                 (progn
                   (setq name (concat name "."))
                   nil)
               ":  <module>"))
            ((equal kind "Type")
             (format ": [%s]" desc))
            ((equal kind "Label")       ;record field projection
             (put-text-property 0 (length name)
                                'ocaml-eglot-completion--sort-early t name)
             (concat ": " desc))
            ((equal kind "Variant")
             (put-text-property 0 (length name)
                                'ocaml-eglot-completion--sort-early t name)
             ;; For variants with no arguments, don't just duplicate the
             ;; name in the description.
             (unless (equal name desc)
               (concat ": " desc)))
            ((equal kind "Constructor")
             (put-text-property 0 (length name)
                                'ocaml-eglot-completion--sort-early t name)
             (concat ": " desc))
            (t (unless (string-empty-p desc) (concat ": " desc)))))
         name)
        (push name ret)))
    ;; Only include labels if there's no Dotted.Context. before the completion
    ;; region
    (when (string-empty-p prefix)
      (seq-doseq (label labels)
        (let ((name (plist-get label :name))
              (type (plist-get label :type)))
          (put-text-property 0 (length name) 'ocaml-eglot-completion--sort-early t name)
          (put-text-property 0 (length name) 'ocaml-eglot-completion--annotation
                             (concat ": " type) name)
          (push name ret)
          (when (and (not (string-empty-p name)) (= (aref name 0) ??))
            (let ((tilde-name (substring name))
                  (tilde-type (string-remove-suffix " option" type)))
              (aset tilde-name 0 ?~)
              (put-text-property 0 (length tilde-name)
                                 'ocaml-eglot-completion--annotation
                                 (concat ": " tilde-type) tilde-name)
              (push tilde-name ret))))))
    (nreverse ret)))

(defun ocaml-eglot-completion--completion-position ()
  "Return the position at which to request completions for the atom at point."
  (let* ((atom-start (car (ocaml-eglot-completion--atom-bounds)))
         ;; By default, we use the start of the atom...
         (position atom-start))
    (unless (memq (char-after atom-start) '(?~ ?? ?`))
      ;; ...except, to work around the lack of
      ;; https://github.com/ocaml/merlin/issues/1751 we can't use the start of
      ;; the atom if we're in a record projection.  Merlin ignores PREFIX when
      ;; determining what record we're projecting from, and only looks at the
      ;; buffer contents.  To support that, lower-case components before point
      ;; in the buffer must come before the position.
      (save-excursion
        ;; Go to the start of the current component...
        (skip-chars-backward "a-z0-9A-Z_'")
        (setq position (point))
        ;; ...and skip backwards over uppercase components only, or components
        ;; starting with a star (to allow for partial-completion globbing)
        (while (and (not (zerop (skip-chars-backward ".")))
                    (not (zerop (skip-chars-backward "a-z0-9A-Z_*'")))
                    (let ((first-char (char-after (point))))
                      (or (eq 'Lu (get-char-code-property first-char 'general-category))
                          (eq first-char ?*))))
          (setq position (point)))))
    position))

(defun ocaml-eglot-completion--display-sort-predicate (a b)
  "Return non-nil if completion candidate A should be displayed before B."
  (let ((a-early (and (not (string-empty-p a))
                      (get-text-property (1- (length a))
                                         'ocaml-eglot-completion--sort-early a)))
        (b-early (and (not (string-empty-p b))
                      (get-text-property (1- (length b))
                                         'ocaml-eglot-completion--sort-early b))))
    (cond
     ((and a-early (not b-early)) t)
     ((and (not a-early) b-early) nil)
     ;; Don't sort early completions alphabetically; merlin returns them
     ;; grouped together by kind.
     ((and a-early b-early) nil)
     (t (string-lessp a b)))))

(defun ocaml-eglot-completion--display-sort (completions)
  "Sort COMPLETIONS for display."
  (sort completions #'ocaml-eglot-completion--display-sort-predicate))

(defun ocaml-eglot-completion--get-completions (prefix &optional buffer)
  "Get (and cache) completions for PREFIX in BUFFER.

Specifically this returns a list of completion candidates, which
don't include PREFIX.

Ideally, point in BUFFER just determines the namespace used for
completion, and PREFIX can be unrelated to whatever symbol is at
point.  Unfortunately, merlin#1751 makes this not true for record
field projections.

Completions are cached in `ocaml-eglot-completion--cache'."
  (with-current-buffer (or buffer (current-buffer))
    (let ((position (ocaml-eglot-completion--completion-position)))
      ;; We request all completions for a context like "List." and filter them
      ;; in Emacs, so that more of the name (or going back to the "List."
      ;; module) doesn't need to re-request completions.
      ;;
      ;; For that to work, the cache key must not depend on the atom being
      ;; completed, only its module prefix, so we only invalidate it when the
      ;; position moves.  This is an approximation, since ocaml-lsp actually
      ;; gets updates to the whole buffer, but it is rarely going to produce
      ;; stale completions.
      (when (or (null ocaml-eglot-completion--cache-position)
                (/= ocaml-eglot-completion--cache-position position))
        (setq-local ocaml-eglot-completion--cache-position position)
        (setq-local ocaml-eglot-completion--cache nil))
      (let ((entry (assoc prefix ocaml-eglot-completion--cache)))
        ;; If the cache didn't satisfy this, send a new request.
        (unless entry
          (setq entry
                (cons prefix
                      ;; Merlin will incorrectly return completions when we have
                      ;; a star in the prefix, even though that's not a valid
                      ;; module name, so just forcibly return nothing.
                      (unless (string-search "*" prefix)
                        (ocaml-eglot-completion--process-completion-data
                         (ocaml-eglot-completion--complete-prefix position prefix)
                         prefix))))
          (push entry ocaml-eglot-completion--cache))
        (cdr entry)))))

(defun ocaml-eglot-completion--get-completions-with-local-quit (context buffer)
  "`ocaml-eglot-completion--get-completions' wrapped in `with-local-quit'.
CONTEXT and BUFFER are passed to `ocaml-eglot-completion--get-completions'.

We use `with-local-quit' in case something in our call stack
binds `inhibit-quit' (for example, timer functions run with
`inhibit-quit' bound to t); without this, our blocking calls to
`accept-process-output' would be uninterruptible and would cause
\"Blocking call to accept-process-output with quit inhibited!!\"
warnings to be messaged.

In practice company already covers this for us: during idle
completion it wraps us in `while-no-input' (see the
:company-use-while-no-input property in
`ocaml-eglot-completion-at-point'), which itself binds
`inhibit-quit' to nil, and manual completion runs from the
command loop where quitting is allowed.  We keep this
wrapper as a defensive guard for other callers of our completion
table that might invoke us with `inhibit-quit' bound."
  (with-local-quit (ocaml-eglot-completion--get-completions context buffer)))

(defun ocaml-eglot-completion--last-position (needle haystack)
  "Get the index of the start of the last occurrence of NEEDLE in HAYSTACK.

NEEDLE must be a string containing a single character."
  (when-let* ((pos (string-search needle (reverse haystack))))
    (- (length haystack) pos)))

(defun ocaml-eglot-completion--split-on-dot (string)
  "Split STRING after its last dot.
Return a cons of the part up to and including the dot and the rest."
  (let ((last-dot-position (ocaml-eglot-completion--last-position "." string)))
    (cons (if last-dot-position (substring string 0 last-dot-position) "")
          (if last-dot-position (substring string last-dot-position) string))))

(defun ocaml-eglot-completion--table (string pred action &optional buffer)
  "Implement completion for OCaml using `completion-at-point' API.

Does programmed completion using STRING, PRED, and ACTION,
see Info node `(elisp)Programmed Completion'.

If BUFFER is non-nil, completes using context from that buffer
instead of the current buffer.

This caches completions in `ocaml-eglot-completion--cache'."
  (let* ((split-on-dot (ocaml-eglot-completion--split-on-dot string))
         (context (car split-on-dot))
         (component (cdr split-on-dot)))
    (cond
     ((eq action 'metadata)
      (list 'metadata
            (cons 'display-sort-function #'ocaml-eglot-completion--display-sort)
            (cons 'annotation-function
                  (when ocaml-eglot-completion-types
                    #'ocaml-eglot-completion--annotate))))
     ((and (consp action) (eq (car action) 'boundaries))
      (let ((start (length context))
            (end (string-search "." (cdr action))))
        ;; include the . in the boundaries
        (when end (setq end (1+ end)))
        (cons 'boundaries (cons start end))))
     ((eq action 'lambda)
      ;; test-completion for "A.B.C." should test that "C." is in the
      ;; completions for "A.B.", not test "" against the completions for
      ;; "A.B.C.", so recalculate context and component accordingly
      (let* ((is-module (string-suffix-p "." string))
             (split-on-dot (ocaml-eglot-completion--split-on-dot
                            (string-remove-suffix "." string)))
             (context (car split-on-dot))
             (component (concat (cdr split-on-dot) (when is-module "."))))
        (let ((completions
               (ocaml-eglot-completion--get-completions-with-local-quit
                context buffer)))
          (test-completion component completions pred))))
     (t
      (let ((completions
             (ocaml-eglot-completion--get-completions-with-local-quit
              context buffer)))
        (cond
         ((null action)
          (let ((ret (try-completion component completions pred)))
            (cond
             ((eq ret t) t)
             ((null ret) nil)
             ;; When try-completion returns a string, that string should cover
             ;; the entire completion region, not just the last component.
             (t (concat context ret)))))
         ((eq action t)
          (let ((all-completions (all-completions component completions pred)))
            ;; If `ocaml-eglot-completion-suppress-ppx-template' is non-nil,
            ;; filter out any identifiers generated by ppx_template.  However,
            ;; if there are no completion results after filtering them out (or
            ;; if the variable was nil to begin with), return all results
            ;; instead.
            (or (and ocaml-eglot-completion-suppress-ppx-template
                     (seq-filter (lambda (cand)
                                   (not (ocaml-eglot-completion--ppx-template-name-p
                                         cand)))
                                 all-completions))
                all-completions)))))))))

(defun ocaml-eglot-completion--atom-bounds ()
  "Return the bounds of the OCaml atom at point, as a cons.

This is like `merlin-bounds-of-ocaml-atom-at-point', but correct.

Also, this function treats \"*\" as a normal alphanumeric
character.  When partial-completion is in `completion-styles' (as
it is by default), this allows including arbitrary globs in the
middle of an OCaml atom and completing over them.

The bounds returned by that function incorrectly omit a \".\"
found at the end of the atom.  Also, it treats \"~foo.bar\" as an
atom."
  (let (atom-start atom-end)
    (save-excursion
      (skip-chars-backward "a-z0-9A-Z*_'")
      ;; Either skip over a single label-starting character, or a
      ;; Dotted.Ocaml.atom
      (when (zerop (skip-chars-backward "?`~" (1- (point))))
        (skip-chars-backward "a-z0-9A-Z*_'."))
      (setq atom-start (point))
      ;; We may not have actually moved backwards at all, in which case we'll be
      ;; returning an atom found after point, possibly a label.
      (if (zerop (skip-chars-forward "?`~" (1+ (point))))
          (skip-chars-forward "a-z0-9A-Z*_'.")
        (skip-chars-forward "a-z0-9A-Z*_'"))
      (setq atom-end (point)))
    (cons atom-start atom-end)))

(defun ocaml-eglot-completion--available-p ()
  "Return non-nil if the server handles `ocamllsp/completePrefixAtPos'.
This request is only available since ocaml-lsp 1.28.1."
  (ocaml-eglot-req--server-capable
   :experimental :ocamllsp :handleCompletePrefixAtPos))

;; Public functions

(defun ocaml-eglot-completion-at-point ()
  "Perform completion at point for OCaml using ocaml-lsp.

This is meant to be added to `completion-at-point-functions' in
buffers managed by Eglot.  If the server does not handle the
`ocamllsp/completePrefixAtPos' request, return nil so that the next
function in `completion-at-point-functions' is used instead.

This completes only on the current fragment; e.g., with point at
the end of \"Aaa.bbb.ccc\", the current fragment is \"ccc\", and
we'll request all completions for \"Aaa.bbb.\" and filter them
down in Emacs.  This means if we backspace and type
\"Aaa.bbb.ddd\" instead, we won't need to re-request completions.

This caches completions between calls as long as we're completing
on the same ocaml atom at the same position, as determined by
`ocaml-eglot-completion--atom-bounds'."
  (let ((atom (ocaml-eglot-completion--atom-bounds)))
    (list (car atom) (cdr atom)
          #'ocaml-eglot-completion--table
          ;; During idle completion, company wraps calls to our completion
          ;; table in `while-no-input', so that our blocking requests don't
          ;; block the user.
          :company-use-while-no-input t
          :exit-function (when ocaml-eglot-completion-types
                           #'ocaml-eglot-completion--exit-function))))

(defun ocaml-eglot-completion-table-for (buffer)
  "Return a completion table for OCaml identifiers in BUFFER.
The completions are those that would be valid at point in BUFFER.

Each completion, except for labelled arguments, has an
`ocaml-eglot-completion-kind' text property saying what kind of
identifier it is.  The value is one of the strings \"Value\",
\"Variant\", \"Label\", \"Module\", \"Modtype\", \"MethodCall\",
\"Keyword\", \"Constructor\" or \"Type\".  Callers can use this to
filter the completions, for example to offer only module names."
  (ocaml-eglot-req--server-capable-or-lose :experimental :ocamllsp :handleCompletePrefixAtPos)
  (lambda (string pred action)
    (ocaml-eglot-completion--table string pred action buffer)))

(provide 'ocaml-eglot-completion)
;;; ocaml-eglot-completion.el ends here
