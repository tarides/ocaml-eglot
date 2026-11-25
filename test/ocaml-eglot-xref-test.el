;;; ocaml-eglot-xref-test.el --- Tests for ocaml-eglot-xref   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2026  The OCaml-eglot Project Contributors
;; Licensed under the MIT license.

;;; Commentary:

;; Buttercup tests for the xref-backend-identifier-at-point implementation.

;;; Code:

(require 'ocaml-eglot-xref)
(require 'buttercup)
(require 'cl-lib)

(defun xref-id-at (text offset)
  "Insert TEXT into a temp buffer, place point at OFFSET, return identifier."
  (with-temp-buffer
    (insert text)
    (goto-char offset)
    (let ((result (xref-backend-identifier-at-point 'ocaml-eglot-xref)))
      (when result
        (substring-no-properties result)))))

(defun lsp-defs-with (preference old-eglot)
  "Call `ocaml-eglot-xref--lsp-definitions' on \"foo\" with PREFERENCE.
If OLD-EGLOT is non-nil, simulate Eglot < 1.26 (where
`eglot--lsp-xrefs-for-method' exists), otherwise simulate Eglot >= 1.26.
The LSP-backed functions are stubbed so that the result describes which
one was called, and with which arguments."
  (let ((ocaml-eglot-locate-preference preference))
    (cl-letf (((symbol-function 'xref-backend-definitions)
               (lambda (backend id) (list 'definitions backend id)))
              ((symbol-function 'xref-backend-xrefs-by-kind)
               (lambda (backend id kind) (list 'by-kind backend id kind)))
              ((symbol-function 'eglot--lsp-xrefs-for-method)
               (and old-eglot
                    (lambda (method &rest _) (list 'for-method method)))))
      (ocaml-eglot-xref--lsp-definitions "foo"))))

(describe "ocaml-eglot-xref"

  (describe "xref-backend-identifier-at-point"

    (describe "regular identifiers"
      (it "finds identifier when point is at the start"
        (expect (xref-id-at "foo" 1) :to-equal "foo"))
      (it "finds identifier when point is in the middle"
        (expect (xref-id-at "foo" 2) :to-equal "foo"))
      (it "finds identifier when point is at the end"
        (expect (xref-id-at "foo" 3) :to-equal "foo"))
      (it "finds identifier with underscores and primes"
        (expect (xref-id-at "my_var'" 1) :to-equal "my_var'"))
      (it "finds identifier with digits"
        (expect (xref-id-at "x42" 1) :to-equal "x42")))

    (describe "module-qualified names"
      (it "finds Module.foo"
        (expect (xref-id-at "Module.foo" 8) :to-equal "Module.foo"))
      (it "finds nested M.N.x"
        (expect (xref-id-at "M.N.x" 5) :to-equal "M.N.x"))
      (it "finds unqualified module name at start of qualified path"
        (expect (xref-id-at "Module.foo" 1) :to-equal "Module")))

    (describe "infix operators"
      (it "finds +"
        (expect (xref-id-at " + " 2) :to-equal "+"))
      (it "finds |>"
        (expect (xref-id-at " |> " 2) :to-equal "|>"))
      (it "finds @@"
        (expect (xref-id-at " @@ " 2) :to-equal "@@"))
      (it "finds :="
        (expect (xref-id-at " := " 2) :to-equal ":=")))

    (describe "prefix operators"
      (it "finds !"
        (expect (xref-id-at " ! " 2) :to-equal "!"))
      (it "finds ?foo style operators"
        (expect (xref-id-at " ?+ " 2) :to-equal "?+")))

    (describe "binding operators"
      (it "finds let+"
        (expect (xref-id-at " let+ " 2) :to-equal "let+"))
      (it "finds and*"
        (expect (xref-id-at " and* " 2) :to-equal "and*"))
      (it "finds let|"
        (expect (xref-id-at " let| " 2) :to-equal "let|")))

    (describe "point at various positions"
      (it "returns nil on whitespace between identifiers"
        (expect (xref-id-at "foo  bar" 5) :to-be nil))
      (it "finds the correct identifier in a sequence"
        (expect (xref-id-at "foo bar" 5) :to-equal "bar"))))

  (describe "ocaml-eglot-xref--lsp-definitions"

    (describe "with ml preference"
      (it "delegates to the Eglot xref-backend-definitions"
        (expect (lsp-defs-with 'ml t) :to-equal '(definitions eglot "foo")))
      (it "does not depend on the Eglot version"
        (expect (lsp-defs-with 'ml nil) :to-equal '(definitions eglot "foo"))))

    (describe "with mli preference"
      (it "uses eglot--lsp-xrefs-for-method with Eglot < 1.26"
        (expect (lsp-defs-with 'mli t)
                :to-equal '(for-method :textDocument/declaration)))
      (it "uses xref-backend-xrefs-by-kind with Eglot >= 1.26"
        (expect (lsp-defs-with 'mli nil)
                :to-equal '(by-kind eglot "foo" declaration))))))

(provide 'ocaml-eglot-xref-test)
;;; ocaml-eglot-xref-test.el ends here
