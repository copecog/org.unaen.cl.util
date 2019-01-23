;;;; org.unaen.cl.util.asd

(asdf:defsystem #:org.unaen.cl.util
  :description "My own collection of Common Lisp utilities."
  :author "Christopher H Cope <christopher.h.cope@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:closer-mop
               #:alexandria)
  :components ((:file "package")
               (:file "util")))
