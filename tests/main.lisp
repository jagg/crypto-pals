(defpackage crypto-pals/tests/main
  (:use :cl
        :crypto-pals
        :rove))
(in-package :crypto-pals/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :crypto-pals)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
