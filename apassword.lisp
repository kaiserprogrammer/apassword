(defpackage :apassword
  (:use :cl)
  (:export
   :hash :check
   :invalid-password :empty-password
   :invalid-hash :empty-hash))
(in-package :apassword)

(defun default-hasher (password)
  (ironclad:pbkdf2-hash-password-to-combined-string
   (ironclad:ascii-string-to-byte-array password)))

(defun default-checker (password hash)
  (unless (and (> (length hash) 6) (string= "PBKDF2" (subseq hash 0 6)))
    (error 'invalid-hash))
  (ironclad:pbkdf2-check-password
   (ironclad:ascii-string-to-byte-array password)
   hash))

(define-condition invalid-password (error)
  ((invalid-password :accessor password :initarg :invalid-password))
  (:report (lambda (c s)
             (format s "Password does not match: ~s" (password c)))))

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
             (format s "An Empty Password Hash was retrieved"))))

(defun blank? (text)
  (not (and text (> (length text) 0))))

(defun corrupt-password? (str)
  (or (not (stringp str))
      (blank? str)))

(defun hash (password &key (hasher #'default-hasher))
  (if (corrupt-password? password)
      (error 'empty-password)
      (funcall hasher password)))

(defun check (password hash &key (checker #'default-checker))
  (if (corrupt-password? password)
      (error 'empty-password)
      (if (corrupt-password? hash)
          (error 'empty-hash)
          (unless (funcall checker password hash)
            (error 'invalid-password :invalid-password password)))))
