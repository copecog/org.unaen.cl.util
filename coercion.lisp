;;;; org.unaen.cl.util/coercion.lisp

(uiop:define-package #:org.unaen.cl.util/coercion
    (:use #:common-lisp)
    (:import-from #:alexandria #:maphash-keys)
    (:export #:vector->list-indices-nil/t
             #:char-interval->list
             #:list->pairs
             #:symbol->list-in-macro))

(in-package #:org.unaen.cl.util/coercion)

(defgeneric vector->list-indices-nil/t (vector)
  (:documentation "Collect unpopulated and populated vector cell indices and return as respective lists.")
  (:method ((v vector))
    (loop :for x :across v
	  :for i :from 0
	  :if x :collect i :into a
	    :else :collect i :into b
	  :finally (return (values b a)))))

(defgeneric char-interval->list (char1 char2)
  (:documentation "Return list of characters on interval between the char1 and char2 character codes inclusive."))

(labels ((char+ (char)
	   (code-char (1+ (char-code char))))
	 (char-interval->list (char1 char2)
	   (loop :for char-iter = char1 :then (char+ char-iter)
		 :collect char-iter
		 :until (char= char-iter char2))))
  
  (defmethod char-interval->list ((char1 character) (char2 character))
    (if (char<= char1 char2)
	(char-interval->list char1 char2)
	(error "Characters do not make a valid interval.")))
  
  (defmethod char-interval->list ((digit1 integer) (digit2 integer))
    (if (<= 0 digit1 digit2 9)
	(char-interval->list (digit-char digit1) (digit-char digit2))
	(error "Digits do not make a valid interval 0-9."))))

(defgeneric list->pairs (source-list)
  (:documentation "Destructure list, pairing off objects into sub-lists.")
  (:method ((source-list list))
    (labels ((pairs-iter (old-list new-list)
               (if (second old-list)
                   (pairs-iter (cddr old-list)
                               (push (list (first old-list) (second old-list))
                                     new-list))
                   new-list)))
      (pairs-iter source-list (list)))))

(defgeneric symbol->list-in-macro (object)
  (:documentation "Take unevaluated object that is either a list or symbol and make sure it is in-list-ed."))

(defmethod symbol->list-in-macro ((object symbol))
  (list object))

(defmethod symbol->list-in-macro ((object list))
  object)

