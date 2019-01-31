;;;; org.unaen.cl.util/syntactic-sugar

(uiop:define-package #:org.unaen.cl.util/syntactic-sugar
    (:use #:common-lisp)
    (:import-from #:alexandria #:maphash-keys)
    (:export #:plusplus
             #:alias
             #:do-hash-keys))

(in-package #:org.unaen.cl.util/syntactic-sugar)
  
;; Bad C habbits...
(defmacro plusplus (place &optional delta-form)
  "Destructively increment place value; However, return the original value."
  (let ((place-before (gensym)))
    `(let ((,place-before ,place))
       (incf ,place ,@(when delta-form (list delta-form)))
       ,place-before)))

(defmacro alias (old-call-name new-call-name)
  "Replace symbol in functional evaluation with another symbol."
  `(defmacro ,new-call-name (&rest args)
     `(,',old-call-name ,@args)))

(defmacro do-hash-keys ((var table &optional result) &body body)
  "Iterate over the keys of a hash-table in similar fashion to dolist."
  `(progn
     (alexandria:maphash-keys #'(lambda (,var)
                                  ,@body)
                              ,table)
     ,result))
