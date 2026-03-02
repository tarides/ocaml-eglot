;;; ocaml-eglot-test.el --- Tests for ocaml-eglot   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2026  The OCaml-eglot Project Contributors
;; Licensed under the MIT license.

;;; Commentary:

;; Buttercup tests for pure helper functions in ocaml-eglot.

;;; Code:

(require 'ocaml-eglot)
(require 'buttercup)

(describe "ocaml-eglot"

  (describe "construct-local-values"
    (it "returns \"none\" when arg is nil and custom is nil"
      (let ((ocaml-eglot-construct-with-local-values nil))
        (expect (ocaml-eglot--construct-local-values nil) :to-equal "none")))
    (it "returns \"local\" when arg is non-nil"
      (let ((ocaml-eglot-construct-with-local-values nil))
        (expect (ocaml-eglot--construct-local-values t) :to-equal "local")))
    (it "returns \"local\" when custom is non-nil"
      (let ((ocaml-eglot-construct-with-local-values t))
        (expect (ocaml-eglot--construct-local-values nil) :to-equal "local")))
    (it "returns \"local\" when both are non-nil"
      (let ((ocaml-eglot-construct-with-local-values t))
        (expect (ocaml-eglot--construct-local-values t) :to-equal "local"))))

  (describe "search-as-key"
    (it "formats name, type, and doc into a single string"
      (let ((result (ocaml-eglot--search-as-key "List.map" "'a -> 'b" "(doc)")))
        (expect result :to-match "List\\.map")
        (expect result :to-match "'a -> 'b")
        (expect result :to-match "(doc)")))
    (it "includes all three parts separated by delimiters"
      (let ((result (ocaml-eglot--search-as-key "f" "int" "")))
        (expect result :to-match "f : int"))))

  (describe "search-as-doc"
    (it "returns empty string when docstring is nil"
      (expect (ocaml-eglot--search-as-doc nil) :to-equal ""))
    (it "returns the first line of a multiline doc"
      (let ((docstring '(:value "First line\nSecond line")))
        (expect (ocaml-eglot--search-as-doc docstring) :to-equal "First line")))
    (it "returns the single line when doc has no newlines"
      (let ((docstring '(:value "Only line")))
        (expect (ocaml-eglot--search-as-doc docstring) :to-equal "Only line")))
    (it "returns empty string when value is empty"
      (let ((docstring '(:value "")))
        (expect (ocaml-eglot--search-as-doc docstring) :to-equal "")))))

(provide 'ocaml-eglot-test)
;;; ocaml-eglot-test.el ends here
