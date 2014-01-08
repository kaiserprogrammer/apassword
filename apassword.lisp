(defpackage :apassword
  (:use :cl)
  (:export
   :hash :check
   :invalid-password :empty-password
   :invalid-hash :empty-hash))
(in-package :apassword)

(defun default-hasher (password)
  (declare (type string password))
  (ironclad:pbkdf2-hash-password-to-combined-string
   (ironclad:ascii-string-to-byte-array password)))

(defun default-checker (password hash)
  (declare (type string password hash))
  (unless (and (> (length hash) 6) (string= "PBKDF2" (subseq hash 0 6)))
    (error 'invalid-hash))
  (ironclad:pbkdf2-check-password
   (ironclad:ascii-string-to-byte-array password)
   hash))

(define-condition invalid-password (error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Password does not match"))))

(define-condition invalid-hash (error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Wrong Hash given to check"))))

(define-condition empty-password (error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "An Empty Password is not allowed"))))

(define-condition empty-hash (error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "An Empty Hash is not allowed"))))

(defun empty? (text)
  (not (and text (> (length text) 0))))

(defun hash (password &key (hasher #'default-hasher))
  "hash a given password to obscure its content so it would take a lot of time to guess it right"
  (if (empty? password)
      (error 'empty-password)
      (funcall hasher password)))


(defun check (password hash &key (checker #'default-checker))
  "given a password and hash it will hash the password and check if its content is the same as the given hash"
  (cond ((empty? password) (error 'empty-password))
        ((empty? hash) (error 'empty-hash))
        ((funcall checker password hash))
        (t (error 'invalid-password))))
