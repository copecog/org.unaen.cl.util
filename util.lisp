;;;; org.unaen.cl.util.lisp

(in-package #:org.unaen.cl.util)

#| ------- with-dot-slots --------------------------------------------------- |#
(defun instance-slots (instance)
  "Function that takes a CLOS instance and returns a list of slot symbols."
  (mapcar #'closer-mop:slot-definition-name
	  (closer-mop:class-slots (class-of instance))))

(defun make-dot-slot-access-pair (instance-symbol slot-symbol)
  "Function that takes a symbol name place for a CLOS instance and slot, then \
interns instance-name.slot-name and returns a list pair: \
(instance-name.slot-name slot-name)"
  (list (intern (string-upcase (concatenate 'string
                                            (symbol-name instance-symbol)
                                            "."
                                            (symbol-name slot-symbol))))
        slot-symbol))

(defun make-dot-slot-access-list (instance-symbol)
  "Function that takes a symbol name place for a CLOS instance and generates \
the list parameter for with-slots to enable .slot shorthand."
  (mapcar #'(lambda (slot-symbol)
	      (make-dot-slot-access-pair instance-symbol slot-symbol))
	  (instance-slots (eval instance-symbol))))
    
(defmacro with-dot-slots (instance &body body)
  "Macro that takes a symbol name place for a CLOS instance and allows access \
of the respective slots using instance-name.slot-name notation."
  `(with-slots ,(make-dot-slot-access-list instance)
       ,instance
     ,@body))

#|
(defclass testclass ()
  ((slot1 :initarg :slot1
	  :initform nil)
   (slot2 :initarg :slot2
	  :initform nil)))

(defvar *foo* (make-instance 'testclass))

(macroexpand-1 '(with-dot-slots *foo* 'body))
|#
