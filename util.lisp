;;;; org.unaen.cl.util.lisp

(in-package #:org.unaen.cl.util)

#| ------- with-dot-slots ---------------------------------------------------- |#
(defun inst->slots-list (inst)
  "Take a CLOS instance and returns a list of slot symbols."
  (mapcar #'closer-mop:slot-definition-name
	  (closer-mop:class-slots (class-of inst))))

(defun inst-slot->inst.slot-slot-pair (inst-sym slot-sym)
  "Takes a symbol name place for a CLOS instance and slot, then interns instance-name.slot-name and returns a list pair: (instance-name.slot-name slot-name)."
  (list (intern (string-upcase (concatenate 'string
                                            (symbol-name inst-sym)
                                            "."
                                            (symbol-name slot-sym))))
        slot-sym))

(defun inst->inst.slot-access-list (inst-sym)
  "Take a symbol name place for a CLOS instance and generates the list parameter for with-slots to enable .slot shorthand."
  (mapcar #'(lambda (slot-sym)
	      (inst-slot->inst.slot-slot-pair inst-sym slot-sym))
	  (inst->slots-list (eval inst-sym))))
    
(defmacro with-dot-slots (instance &body body)
  "Takes a symbol name place for a CLOS instance, and body of forms, and allow access of the instances slots using instance-name.slot-name notation in body."
  `(with-slots ,(inst->inst.slot-access-list instance)
       ,instance
     ,@body))

#| ------- make-func-bind-value-list ----------------------------------------- |#

(defmacro sym-func+val (func-val-pairs)
  `',(mapcar #'(lambda (func-val-pair)
                 (list (first func-val-pair)
                    (list (first func-val-pair)
                          (second func-val-pair))))
             func-val-pairs))

#| ------- copy-all ---------------------------------------------------------- |#
;;; Ignoring existing copy functions
(defgeneric copy-all (object)
  (:documentation "A recursive generic function with a method for every built in Common Lisp type to produce a copy."))

(defmethod copy-all ((obj cons))
  "(Mutable) Cons Cells: LIST (not (null '(a list))), CONS"
  (loop :for obj->car :in obj :collect (copy-all obj->car)))

(defmethod copy-all ((obj vector))
  "(Mutable) Vectors: [SIMPLE|BIT|SIMPLE-BIT]-?VECTOR, [SIMPLE|BASE|SIMPLE-BASE]-?STRING"
  (let* ((#1=array-dimensions (#1# obj))
         (#2=array-element-type (#2# obj))
         (#3=adjustable-array-p (#3# obj))
         (#4=array-has-fill-pointer-p (#4# obj))
	 (#5=fill-pointer (when #4# (#5# obj))))
    (loop :with new-vector = (make-array array-dimensions
					 :element-type array-element-type
					 :adjustable adjustable-array-p
					 :fill-pointer fill-pointer)
	  :for cell-data :across obj
	  :for cell-int :from 0 :to (if array-has-fill-pointer-p
					fill-pointer
					(first array-dimensions))
	  :do (setf (aref new-vector cell-int)
		    (copy-all cell-data))
	  :finally (return new-vector))))

(defmethod copy-all ((obj hash-table))
  "(Mutable) Hash Tables: HASH-TABLE"
  (loop :with new-hash-table = (make-hash-table :test (hash-table-test obj)
						:size (hash-table-size obj)
						:rehash-size (hash-table-rehash-size obj)
						:rehash-threshold (hash-table-rehash-threshold obj))
	:for hash-key :being :the :hash-keys :of obj :using (:hash-value hash-value)
	:do
	   (setf (gethash (copy-all hash-key) new-hash-table)
		 (copy-all hash-value))
	:finally (return new-hash-table)))

(defmethod copy-all ((obj symbol))
  "(Immutable) Symbols: SYMBOL, NULL (reduce #'eq '() () nil 'nil), KEYWORD, BOOLEAN"
  obj)

(defmethod copy-all ((obj number))
  "(Immutable) Numbers: NUMBER, COMPLEX, REAL, [SHORT|SINGLE|DOUBLE|LONG]-?FLOAT, RATIONAL, RATIO, INTEGER, [FIX|BIG]NUM, [SIGNED|UNSIGNED]-BYTE, BIT"
  obj)

(defmethod copy-all ((obj character))
  "(Immutable) Characters: CHARACTER, [EXTENDED|BASE|STANDARD]-CHAR"
  obj)

#| ------- Misc -------------------------------------------------------------- |#
(defun file-contents->string (filename)
  "Function to read file contents and return as a single string."
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun file-lines->list-of-strings (filename)
  "Function to read file contents and return lines as list of strings."
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defgeneric vector->list-indices-nil/t (vector)
  (:documentation "Collect unpopulated and populated vector cell indices and return as respective lists.")
  (:method ((v vector))
    (loop
       :for x :across v
       :for i :from 0
       :if x :collect i :into a
       :else :collect i :into b
       :finally (return (values b a)))))

(defgeneric char-interval->list (char1 char2)
  (:documentation "Return list of characters on interval between the char1 and char2 character codes inclusive.")
  (:method ((char1 character) (char2 character))
    (when (char< char1 char2)
      (do ((char-iter char1 (code-char (1+ (char-code char-iter))))
           (char-list (list) (push char-iter char-list)))
          ((char> char-iter char2) (nreverse char-list))))))

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

(defgeneric separate-if (predicate-function sequence &rest rest)
  (:documentation "Separate into not matching and matching lists according to the predicate-function.")
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
  
(defgeneric symbol->list-in-macro (object)
  (:documentation "Take unevaluated object that is either a list or symbol and make sure it is in-list-ed."))

(defmethod symbol->list-in-macro ((object symbol))
  (list object))

(defmethod symbol->list-in-macro ((object list))
  object)

;; Bad C habbits...
(defmacro plusplus (place &optional delta-form)
  "Destructively increment place value; However, return the original value."
  (let ((place-before (gensym)))
    `(let ((,place-before ,place))
       (incf ,place ,@(when delta-form (list delta-form)))
       ,place-before)))

(defun truth (ignored-var)
  "Accept a single required value and always return true."
  (declare (ignore ignored-var))
  t)

(defun false (ignored-var)
  "Accept a single required value and always return false."
  (declare (ignore ignored-var))
  nil)

(defmacro alias (old-call-name new-call-name)
  "Replace symbol in functional evaluation with another symbol."
  `(defmacro ,new-call-name (&rest args)
     `(,',old-call-name ,@args)))

