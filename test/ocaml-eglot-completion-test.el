;;; ocaml-eglot-completion-test.el --- Tests for ocaml-eglot-completion   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2026  The OCaml-eglot Project Contributors
;; Licensed under the MIT license.

;;; Commentary:

;; Buttercup tests for ocaml-eglot-completion that don't need a running
;; ocaml-lsp server.

;;; Code:

(require 'ocaml-eglot-completion)
(require 'cl-lib)
(require 'buttercup)

(defun ocaml-eglot-completion-test--split-on-boundaries (prefix suffix table)
  "Split PREFIX and SUFFIX using `completion-boundaries' from TABLE.
Return a list of the string before the boundaries, within the
boundaries, and after the boundaries."
  (let ((boundaries (completion-boundaries prefix table nil suffix)))
    (list
     (substring-no-properties prefix 0 (car boundaries))
     (concat (substring-no-properties prefix (car boundaries))
             (substring-no-properties suffix 0 (cdr boundaries)))
     (substring-no-properties suffix (cdr boundaries)))))

(defun ocaml-eglot-completion-test--regions (prefix suffix)
  "Split PREFIX and SUFFIX with `ocaml-eglot-completion-at-point'.
This is as if point was between PREFIX and SUFFIX.  Return:
- the part of the completion region before the position at which
  we request completions
- the part of the completion region before the current completion
  boundaries, which is what will be sent to ocaml-lsp as the prefix
- the contents of the current completion boundaries
- the part of the OCaml atom after the current completion boundaries"
  (with-temp-buffer
    (insert "?before-ignored  ")
    (insert prefix)
    (save-excursion (insert suffix) (insert "?after-ignored  "))
    (let ((cap (let ((ocaml-eglot-completion-dot-after-module t))
                 (ocaml-eglot-completion-at-point))))
      (cons
       (buffer-substring-no-properties
        (nth 0 cap) (ocaml-eglot-completion--completion-position))
       (ocaml-eglot-completion-test--split-on-boundaries
        (buffer-substring-no-properties (nth 0 cap) (point))
        (buffer-substring-no-properties (point) (nth 1 cap))
        (nth 2 cap))))))

(describe "ocaml-eglot-completion"

  (describe "completion regions"
      (it "Aaa.bbb.c|cc.ddd"
        (expect (ocaml-eglot-completion-test--regions "Aaa.bbb.c" "cc.ddd")
                :to-equal '("Aaa.bbb." "Aaa.bbb." "ccc." "ddd")))
      (it "~fo|o.bar"
        (expect (ocaml-eglot-completion-test--regions "~fo" "o.bar")
                :to-equal '("" "" "~foo" "")))
      (it "|~foo.bar"
        (expect (ocaml-eglot-completion-test--regions "" "~foo.bar")
                :to-equal '("" "" "~foo" "")))
      (it "~fo|o~bar"
        (expect (ocaml-eglot-completion-test--regions "~fo" "o~bar")
                :to-equal '("" "" "~foo" "")))
      (it "~foo|~bar"
        (expect (ocaml-eglot-completion-test--regions "~foo" "~bar")
                :to-equal '("" "" "~foo" "")))
      (it "~fo|o.b~ar"
        (expect (ocaml-eglot-completion-test--regions "~fo" "o.b~ar")
                :to-equal '("" "" "~foo" "")))
      ;; There's no obvious correct thing to return in this case, so this is fine.
      (it "~foo.bar|"
        (expect (ocaml-eglot-completion-test--regions "~foo.bar" "")
                :to-equal '("foo." "foo." "bar" "")))
      (it "|~"
        (expect (ocaml-eglot-completion-test--regions "" "~")
                :to-equal '("" "" "~" "")))
      (it "|Aaa.bbb.ccc.ddd"
        (expect (ocaml-eglot-completion-test--regions "" "Aaa.bbb.ccc.ddd")
                :to-equal '("" "" "Aaa." "bbb.ccc.ddd")))
      (it "A|aa.bbb.ccc.ddd"
        (expect (ocaml-eglot-completion-test--regions "A" "aa.bbb.ccc.ddd")
                :to-equal '("" "" "Aaa." "bbb.ccc.ddd")))
      ;; An "atom" can also just be a dotted path projecting from an expression
      (it "(foo bar).|"
        (expect (ocaml-eglot-completion-test--regions "(foo bar)." "")
                :to-equal '("." "." "" "")))
      (it "(foo bar).Aa|a"
        (expect (ocaml-eglot-completion-test--regions "(foo bar).Aa" "a")
                :to-equal '("." "." "Aaa" "")))
      (it "(foo bar).Aaa.Bb|b.ccc"
        (expect (ocaml-eglot-completion-test--regions "(foo bar).Aaa.Bb" "b.ccc")
                :to-equal '("." ".Aaa." "Bbb." "ccc")))
      (it "(foo bar).Aaa.bb|b.ccc"
        (expect (ocaml-eglot-completion-test--regions "(foo bar).Aaa.bb" "b.ccc")
                :to-equal '("." ".Aaa." "bbb." "ccc")))
      (it "(foo bar).aaa.bb|b.ccc"
        (expect (ocaml-eglot-completion-test--regions "(foo bar).aaa.bb" "b.ccc")
                :to-equal '(".aaa." ".aaa." "bbb." "ccc")))
      ;; We should skip back over only uppercase components before point, not lowercase ones
      (it "M.|x"
        (expect (ocaml-eglot-completion-test--regions "M." "x")
                :to-equal '("" "M." "x" "")))
      (it "M.t.|x"
        (expect (ocaml-eglot-completion-test--regions "M.t." "x")
                :to-equal '("M.t." "M.t." "x" "")))
      (it "M.N.|x"
        (expect (ocaml-eglot-completion-test--regions "M.N." "x")
                :to-equal '("" "M.N." "x" "")))
      (it "M.t.N.|x"
        (expect (ocaml-eglot-completion-test--regions "M.t.N." "x")
                :to-equal '("M.t." "M.t.N." "x" "")))
      (it "aa.bB.CC.x|"
        (expect (ocaml-eglot-completion-test--regions "aa.bB.CC.x" "")
                :to-equal '("aa.bB." "aa.bB.CC." "x" "")))
      (it "Aa.bB.CC.x|"
        (expect (ocaml-eglot-completion-test--regions "Aa.bB.CC.x" "")
                :to-equal '("Aa.bB." "Aa.bB.CC." "x" "")))
      (it "aa.Bb.cc.x|"
        (expect (ocaml-eglot-completion-test--regions "aa.Bb.cc.x" "")
                :to-equal '("aa.Bb.cc." "aa.Bb.cc." "x" "")))
      (it "aa.Bb.Cc.x|"
        (expect (ocaml-eglot-completion-test--regions "aa.Bb.Cc.x" "")
                :to-equal '("aa." "aa.Bb.Cc." "x" ""))))

  (describe "cancel-on-input"
    ;; During idle completion, Company and Corfu wrap us in
    ;; `while-no-input', but `jsonrpc-request' binds `throw-on-input' to nil,
    ;; so `ocaml-eglot-completion--complete-prefix' has to implement the
    ;; interruption itself.  We simulate the `while-no-input' by binding
    ;; `throw-on-input' the way that macro does, so that the test doesn't
    ;; depend on the real input state.
    (it "aborts the enclosing while-no-input when a request is canceled"
      (let ((requests nil))
        (cl-letf (((symbol-function 'ocaml-eglot-req--TextDocumentIdentifier)
                   (lambda () '(:uri "file:///fake.ml")))
                  ((symbol-function 'ocaml-eglot-req--send)
                   (lambda (_method _params &rest args)
                     (let ((cancel-on-input (plist-get args :cancel-on-input)))
                       (push cancel-on-input requests)
                       (if cancel-on-input
                           ;; Simulate input arriving while waiting for the reply.
                           (plist-get args :cancel-on-input-retval)
                         '(:entries [(:label "vvaa" :kind 12 :detail "ttbb")]))))))
          (with-temp-buffer
            (insert "Mmaa.")
            ;; Outside `while-no-input', block until the server replies.
            (expect (ocaml-eglot-completion--get-completions "Mmaa.")
                    :to-equal '("vvaa"))
            (expect requests :to-equal '(nil))
            (setq-local ocaml-eglot-completion--cache nil)
            ;; Inside `while-no-input', a request interrupted by user input
            ;; aborts the whole form, and no bogus result is cached.
            (expect (catch 'test-tag
                      (let ((throw-on-input 'test-tag))
                        (ocaml-eglot-completion--get-completions "Mmaa.")))
                    :to-be t)
            (expect requests :to-equal '(t nil))
            (expect ocaml-eglot-completion--cache :to-be nil)))))))

(provide 'ocaml-eglot-completion-test)
;;; ocaml-eglot-completion-test.el ends here
