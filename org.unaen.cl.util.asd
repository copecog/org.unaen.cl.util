;;;; org.unaen.cl.util/org.unaen.cl.util.asd

(defsystem "org.unaen.cl.util"
  :description "My own collection of Common Lisp utilities."
  :author "Christopher H Cope <christopher.h.cope@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :class :package-inferred-system
  :depends-on ("closer-mop"
               "alexandria"
               "org.unaen.cl.util/util"))
