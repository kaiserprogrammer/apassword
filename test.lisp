(defpackage :apassword-test
  (:use :cl :apassword :lisp-unit2))
(in-package :apassword-test)

(lisp-unit2:remove-tests)

(define-test hash-unequal-to-password ()
  (assert-false (string= "secret" (hash "secret"))))

(define-test verifying-password ()
  (check "secret" (hash "secret"))
  (assert-error 'invalid-hash (check "wrong" "wrong"))
  (assert-error 'invalid-password (check "wrong" (hash "secret"))))

(define-test blank-passwords ()
  (assert-error 'empty-hash (check "blub" ""))
  (assert-error 'empty-password (hash "")))

(with-test-results ()
  (run-tests))
