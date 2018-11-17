;;;; org.unaen.src.cl.with-dot-slots.asd

(asdf:defsystem #:org.unaen.src.cl.with-dot-slots
  :description "Utility macro to access CLOS objects with dot accessors."
  :author "Christopher H Cope <christopher.h.cope@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:closer-mop)
  :components ((:file "package")
               (:file "with-dot-slots")))
