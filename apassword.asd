(asdf:defsystem apassword
  :version "0"
  :description "Provides hash and check for passwords"
  :maintainer "Jürgen Bickert <juergenbickert@gmail.com>"
  :author "Jürgen Bickert <juergenbickert@gmail.com>"
  :licence "BSD-style"
  :depends-on(ironclad)
  :serial t
  ;; components likely need manual reordering
  :components ((:file "apassword"))
  ;; :long-description ""
  )
