;;; ocaml-eglot-xref-test.el --- Tests for ocaml-eglot-xref   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2026  The OCaml-eglot Project Contributors
;; Licensed under the MIT license.

;;; Commentary:

;; Buttercup tests for the xref-backend-identifier-at-point implementation.

;;; Code:

(require 'ocaml-eglot-xref)
(require 'buttercup)

(defun xref-id-at (text offset)
  "Insert TEXT into a temp buffer, place point at OFFSET, return identifier."
  (with-temp-buffer
    (insert text)
    (goto-char offset)
    (let ((result (xref-backend-identifier-at-point 'ocaml-eglot-xref)))
      (when result
        (substring-no-properties result)))))

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
        (expect (xref-id-at "foo bar" 5) :to-equal "bar")))))

(provide 'ocaml-eglot-xref-test)
;;; ocaml-eglot-xref-test.el ends here
