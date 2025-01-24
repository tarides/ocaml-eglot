;;; ocaml-eglot-util-test.el --- Test for ocaml-eglot-util   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2024  The OCaml-eglot Project Contributors
;; Licensed under the MIT license.

;;; Commentary:

;; Just some test for ocaml-eglot-util

;;; Code:

(require 'ocaml-eglot-util)
(require 'cl-lib)
(require 'ert)

(defgroup ocaml-eglot-util-test nil
  "Test for ocaml-eglot-util."
  :group 'test)

(ert-deftest empty-p-illustration-test ()
  "Test for illustrating `ocaml-eglot-util--empty-p' behaviour."
  (should-not (ocaml-eglot-util--empty-p [1 2 3 4]))
  ;; (should-not (ocaml-eglot-util--empty-p [1]))
  ;; (should (ocaml-eglot-util--empty-p []))
  )

(ert-deftest vec-first-or-nil-test ()
  "Test for `ocaml-eglot-util--vec-first-or-nil'."
  (should-not (ocaml-eglot-util--vec-first-or-nil []))
  (should (= (ocaml-eglot-util--vec-first-or-nil [1]) 1))
  (should (= (ocaml-eglot-util--vec-first-or-nil [2 3 4 5]) 2)))

(defun pos (l c)
  "Builds a position with a line number (L) and a character number (C)."
  `(:line ,l :character ,c))

(defun same-pos? (a b)
  "Return non-nil if A = B, nil otherwise."
  (= (ocaml-eglot-util--compare-position a b) 0))

(ert-deftest compare-position-test ()
  "Test for `ocaml-eglot-util--compare-position'."
  (should (= (ocaml-eglot-util--compare-position (pos 1 1) (pos 0 0)) 1))
  (should (= (ocaml-eglot-util--compare-position (pos 1 1) (pos 1 1)) 0))
  (should (= (ocaml-eglot-util--compare-position (pos 0 0) (pos 1 1)) -1))
  (should (= (ocaml-eglot-util--compare-position (pos 2 3) (pos 2 2)) 1))
  (should (= (ocaml-eglot-util--compare-position (pos 2 2) (pos 2 2)) 0))
  (should (= (ocaml-eglot-util--compare-position (pos 7 7) (pos 7 8)) -1)))

(ert-deftest position-increase-char-test ()
  "Test for `ocaml-eglot-util--position-increase-char'."
  (should (same-pos?
           (ocaml-eglot-util--position-increase-char (pos 1 1) "")
           (pos 1 1)))
  (should (same-pos?
           (ocaml-eglot-util--position-increase-char (pos 1 1) "foo")
           (pos 1 4))))

(ert-deftest is-interface-test ()
  "Test for `ocaml-eglot-util--is-interface'."
  (should-not (ocaml-eglot-util--is-interface "file:///t.ml"))
  (should-not (ocaml-eglot-util--is-interface "file:///t.re"))
  (should-not (ocaml-eglot-util--is-interface "file:///t.eliom"))
  (should (ocaml-eglot-util--is-interface "file:///t.mli"))
  (should (ocaml-eglot-util--is-interface "file:///t.rei"))
  (should (ocaml-eglot-util--is-interface "file:///t.eliomi")))


(provide 'ocaml-eglot-util-test)
;;; ocaml-eglot-util-test.el ends here
