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
      (expect (ocaml-eglot-util--is-interface "file:///t.eliomi") :to-be-truthy)))

  (describe "text-less-than"
    (it "returns t for text with fewer newlines than limit"
      (expect (ocaml-eglot-util--text-less-than "hello" 3) :to-be-truthy)
      (expect (ocaml-eglot-util--text-less-than "a\nb" 3) :to-be-truthy))
    (it "returns t when newline count equals limit"
      (expect (ocaml-eglot-util--text-less-than "a\nb\nc" 2) :to-be-truthy))
    (it "returns nil when newline count exceeds limit"
      (expect (ocaml-eglot-util--text-less-than "a\nb\nc\nd" 2) :not :to-be-truthy))
    (it "returns t for empty text"
      (expect (ocaml-eglot-util--text-less-than "" 0) :to-be-truthy)))

  (describe "merlin-pos-to-lsp-pos"
    (it "converts 1-based line to 0-based"
      (let ((result (ocaml-eglot-util--merlin-pos-to-lsp-pos '(:line 1 :col 0))))
        (expect (cl-getf result :line) :to-equal 0)
        (expect (cl-getf result :character) :to-equal 0)))
    (it "preserves column value"
      (let ((result (ocaml-eglot-util--merlin-pos-to-lsp-pos '(:line 5 :col 10))))
        (expect (cl-getf result :line) :to-equal 4)
        (expect (cl-getf result :character) :to-equal 10))))

  (describe "point-by-pos"
    (it "returns correct point for line and column"
      (with-temp-buffer
        (insert "hello\nworld\n")
        (expect (ocaml-eglot-util--point-by-pos 1 0) :to-equal 1)
        (expect (ocaml-eglot-util--point-by-pos 2 0) :to-equal 7)))
    (it "handles column offsets"
      (with-temp-buffer
        (insert "abcdef\nghijkl\n")
        (expect (ocaml-eglot-util--point-by-pos 1 3) :to-equal 4)
        (expect (ocaml-eglot-util--point-by-pos 2 2) :to-equal 10))))

  (describe "pos-to-point"
    (it "converts a merlin pos to buffer point"
      (with-temp-buffer
        (insert "first\nsecond\n")
        (expect (ocaml-eglot-util--pos-to-point '(:line 1 :col 0)) :to-equal 1)
        (expect (ocaml-eglot-util--pos-to-point '(:line 2 :col 0)) :to-equal 7))))

  (describe "current-range-or-nil"
    (it "returns nil when no region is active"
      (with-temp-buffer
        (insert "hello")
        (expect (ocaml-eglot-util--current-range-or-nil) :to-be nil)))
    (it "returns a range when region is active"
      (with-temp-buffer
        (insert "hello world")
        (goto-char 1)
        (set-mark 1)
        (goto-char 6)
        (activate-mark)
        (let ((range (ocaml-eglot-util--current-range-or-nil)))
          (expect range :not :to-be nil)
          (expect (cl-getf range :start) :not :to-be nil)
          (expect (cl-getf range :end) :not :to-be nil)))))

  (describe "current-range"
    (it "returns a single-char range when no region is active"
      (with-temp-buffer
        (insert "hello")
        (goto-char 1)
        (let ((range (ocaml-eglot-util--current-range)))
          (expect (cl-getf range :start) :not :to-be nil)
          (expect (cl-getf range :end) :not :to-be nil))))
    (it "returns region range when region is active"
      (with-temp-buffer
        (insert "hello world")
        (goto-char 1)
        (set-mark 1)
        (goto-char 6)
        (activate-mark)
        (let ((range (ocaml-eglot-util--current-range)))
          (expect (cl-getf range :start) :not :to-be nil)
          (expect (cl-getf range :end) :not :to-be nil))))))

(provide 'ocaml-eglot-util-test)
;;; ocaml-eglot-util-test.el ends here
