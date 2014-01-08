# APassword

```lisp
(check "password" (hash "password")) => nil
(check "invalid" (hash "password")) => error
```

```lisp
(define-test hash-unequal-to-password ()
  (assert-false (string= "secret" (hash "secret"))))

(define-test verifying-password ()
  (check "secret" (hash "secret"))
  (assert-error 'invalid-hash (check "wrong" "wrong"))
  (assert-error 'invalid-password (check "wrong" (hash "secret"))))

(define-test blank-passwords ()
  (assert-error 'empty-hash (check "blub" ""))
  (assert-error 'empty-password (hash "")))
```
