;;;; org.unaen.cl.util/util.lisp

(uiop:define-package #:org.unaen.cl.util
    (:use #:common-lisp)
    (:use-reexport #:org.unaen.cl.util/with-dot-slots
                   #:org.unaen.cl.util/multiple-value-call
                   #:org.unaen.cl.util/copy
                   #:org.unaen.cl.util/file-contents
                   #:org.unaen.cl.util/coercion
                   #:org.unaen.cl.util/predicate
                   #:org.unaen.cl.util/syntactic-sugar))

(in-package #:org.unaen.cl.util)

