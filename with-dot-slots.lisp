;;;; org.unaen.cl.util/with-dot-slots.lisp

(uiop:define-package #:org.unaen.cl.util/with-dot-slots
    (:use #:common-lisp)
    (:shadowing-import-from #:closer-mop #:slot-definition-name #:class-slots)
    (:export #:inst->slots-list
             #:with-dot-slots))

(in-package #:org.unaen.cl.util/with-dot-slots)

(defun inst->slots-list (inst)
  "Take a CLOS instance and returns a list of slot symbols."
  (mapcar #'closer-mop:slot-definition-name
	  (closer-mop:class-slots (class-of inst))))

(defun inst-slot->inst.slot-slot-pair (inst-sym slot-sym)
  #.(format nil "Takes a symbol name place for a CLOS instance and slot, then ~
                 interns instance-name.slot-name and returns a list pair: ~
                 (instance-name.slot-name slot-name).")
  (list (intern (string-upcase (concatenate 'string
                                            (symbol-name inst-sym)
                                            "."
                                            (symbol-name slot-sym))))
        slot-sym))

(defun inst->inst.slot-access-list (inst-sym)
  #.(format nil "Take a symbol name place for a CLOS instance and generates the ~
                 list parameter for with-slots to enable .slot shorthand.")
  (mapcar #'(lambda (slot-sym)
	      (inst-slot->inst.slot-slot-pair inst-sym slot-sym))
	  (inst->slots-list (eval inst-sym))))
    
(defmacro with-dot-slots (instance &body body)
  #.(format nil "Takes a symbol name place for a CLOS instance, and body of ~
                 forms, and allow access of the instances slots using ~
                 instance-name.slot-name notation in body. ONLY USE ON REPL.")
  `(with-slots ,(inst->inst.slot-access-list instance)
       ,instance
     ,@body))
