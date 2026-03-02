;;; ocaml-eglot-util-test.el --- Tests for ocaml-eglot-util   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2024-2026  The OCaml-eglot Project Contributors
;; Licensed under the MIT license.

;;; Commentary:

;; Buttercup tests for ocaml-eglot-util.

;;; Code:

(require 'ocaml-eglot-util)
(require 'cl-lib)
(require 'buttercup)

(defun pos (l c)
  "Build a position with line number L and character number C."
  `(:line ,l :character ,c))

(defun same-pos? (a b)
  "Return non-nil if positions A and B are equal."
  (= (ocaml-eglot-util--compare-position a b) 0))

(describe "ocaml-eglot-util"

  (describe "vec-first-or-nil"
    (it "returns nil for an empty vector"
      (expect (ocaml-eglot-util--vec-first-or-nil []) :to-be nil))
    (it "returns the single element of a one-element vector"
      (expect (ocaml-eglot-util--vec-first-or-nil [1]) :to-equal 1))
    (it "returns the first element of a multi-element vector"
      (expect (ocaml-eglot-util--vec-first-or-nil [2 3 4 5]) :to-equal 2)))

  (describe "compare-position"
    (it "returns 1 when first position is greater"
      (expect (ocaml-eglot-util--compare-position (pos 1 1) (pos 0 0)) :to-equal 1))
    (it "returns 0 for equal positions"
      (expect (ocaml-eglot-util--compare-position (pos 1 1) (pos 1 1)) :to-equal 0))
    (it "returns -1 when first position is lesser"
      (expect (ocaml-eglot-util--compare-position (pos 0 0) (pos 1 1)) :to-equal -1))
    (it "compares by character when lines are equal"
      (expect (ocaml-eglot-util--compare-position (pos 2 3) (pos 2 2)) :to-equal 1)
      (expect (ocaml-eglot-util--compare-position (pos 2 2) (pos 2 2)) :to-equal 0)
      (expect (ocaml-eglot-util--compare-position (pos 7 7) (pos 7 8)) :to-equal -1)))

  (describe "position-increase-char"
    (it "does not change position for empty string"
      (expect (same-pos?
               (ocaml-eglot-util--position-increase-char (pos 1 1) "")
               (pos 1 1))
              :to-be-truthy))
    (it "increases character by string length"
      (expect (same-pos?
               (ocaml-eglot-util--position-increase-char (pos 1 1) "foo")
               (pos 1 4))
              :to-be-truthy)))

  (describe "is-interface"
    (it "returns nil for implementation files"
      (expect (ocaml-eglot-util--is-interface "file:///t.ml") :not :to-be-truthy)
      (expect (ocaml-eglot-util--is-interface "file:///t.re") :not :to-be-truthy)
      (expect (ocaml-eglot-util--is-interface "file:///t.eliom") :not :to-be-truthy))
    (it "returns truthy for interface files"
      (expect (ocaml-eglot-util--is-interface "file:///t.mli") :to-be-truthy)
      (expect (ocaml-eglot-util--is-interface "file:///t.rei") :to-be-truthy)
      (expect (ocaml-eglot-util--is-interface "file:///t.eliomi") :to-be-truthy))))

(provide 'ocaml-eglot-util-test)
;;; ocaml-eglot-util-test.el ends here
