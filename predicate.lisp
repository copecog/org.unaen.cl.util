;;;; org.unaen.cl.util/predicate.lisp

(uiop:define-package #:org.unaen.cl.util/predicate
    (:use #:common-lisp)
    (:export #:separate-if
             #:truth
             #:false))

(in-package #:org.unaen.cl.util/predicate)

(defgeneric separate-if (predicate-function sequence &rest rest)
  (:documentation "Separate into not matching and matching lists according to 
the predicate-function.")
  (:method ((predicate function) (sequence sequence) &rest rest)
    (let ((matched (list)))
      (values (apply #'remove-if
                     #'(lambda (x)
                         (let ((it (funcall predicate x)))
                           (unless it
                             (push x matched))))
                     sequence
                     rest)
              (nreverse matched)))))
  
(defun truth (ignored-var)
  "Accept a single required value and always return true."
  (declare (ignore ignored-var))
  t)

(defun false (ignored-var)
  "Accept a single required value and always return false."
  (declare (ignore ignored-var))
  nil)

