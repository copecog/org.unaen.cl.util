;;;; org.unaen.cl.util.lisp

(uiop:define-package #:org.unaen.cl.util/util
    (:use #:common-lisp)
    (:shadowing-import-from #:closer-mop #:slot-definition-name #:class-slots)
    (:shadowing-import-from #:alexandria #:maphash-keys)
    (:export #:inst->slots-list
             #:with-dot-slots
             #:copy-all
             #:copy-depth
             #:file-contents->string
             #:file-lines->list-of-strings
             #:vector->list-indices-nil/t
             #:char-interval->list
             #:list->pairs
             #:separate-if
             #:symbol->list-in-macro
             #:plusplus
             #:truth
             #:false
             #:alias
             #:do-hash-keys))

(in-package #:org.unaen.cl.util/util)

#| ------- with-dot-slots ---------------------------------------------------- |#
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

#| ------- multiple-value-call ----------------------------------------------- |#
(defun |#V-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((func-exp (read stream t nil t)))
    `(multiple-value-call #',(car func-exp) ,@(cdr func-exp))))

(set-dispatch-macro-character #\# #\V #'|#V-reader|)

;;; Rest via https://gist.github.com/informatimago (thanks!)
(defun |⸨mvc⸩| (stream char)
  (declare (ignore char))
  (let ((call (read-delimited-list #\⸩ stream t)))
    `(multiple-value-call (function ,(first call)) ,@(rest call))))

(set-macro-character #\⸨ (function |⸨mvc⸩|))

#| Emacs Lisp key binding for "⸨", "⸩"
(local-set-key (kbd "A-(")
               (lambda (rep)
                 (interactive "p")
                 (insert (make-string rep ?⸨))))

(local-set-key (kbd "A-)")
               (lambda (rep)
                 (interactive "p")
                 (insert (make-string rep ?⸩))))
|#

#| ------- copy-all ---------------------------------------------------------- |#
;;; Ignoring existing copy functions
(defgeneric copy-all (object)
  (:documentation "A recursive generic function with a method for every built in 
Common Lisp type to produce a copy."))

(defmethod copy-all ((obj cons))
  "(Mutable) Cons Cells: LIST (not (null '(a list))), CONS"
  (loop :for obj->car :in obj :collect (copy-all obj->car)))

(defmethod copy-all ((obj vector))
  "(Mutable) Vectors: [SIMPLE|BIT|SIMPLE-BIT]-?VECTOR, 
[SIMPLE|BASE|SIMPLE-BASE]-?STRING"
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
  "(Immutable) Symbols: SYMBOL, NULL (reduce #'eq '() () nil 'nil), KEYWORD, 
BOOLEAN"
  obj)

(defmethod copy-all ((obj number))
  "(Immutable) Numbers: NUMBER, COMPLEX, REAL, [SHORT|SINGLE|DOUBLE|LONG]-?FLOAT, 
RATIONAL, RATIO, INTEGER, [FIX|BIG]NUM, [SIGNED|UNSIGNED]-BYTE, BIT"
  obj)

(defmethod copy-all ((obj character))
  "(Immutable) Characters: CHARACTER, [EXTENDED|BASE|STANDARD]-CHAR"
  obj)

#| ------- copy-depth -------------------------------------------------------- |#
(declaim (inline copy-depth))
(defun copy-depth (object depth)
  (declare (type (integer 0 *) depth))
  (if (= 0 depth)
      object
      (copy-depth-rec object
                      (1- depth))))

(defgeneric copy-depth-rec (object depth)
  (:documentation "A recursive generic function with a method for every built in 
Common Lisp type to produce a copy."))

(defmethod copy-depth-rec :before (object depth)
  (declare (type (integer 0 *) depth)))

(defmethod copy-depth-rec ((obj cons) (depth integer))
  "(Mutable) Cons Cells: LIST (not (null '(a list))), CONS"
  (loop
     :for obj->car :in obj :collect (if (= 0 depth)
                                        obj->car
                                        (copy-depth-rec obj->car
                                                        (1- depth)))))

(defmethod copy-depth-rec ((obj vector) (depth integer))
  "(Mutable) Vectors: [SIMPLE|BIT|SIMPLE-BIT]-?VECTOR, 
[SIMPLE|BASE|SIMPLE-BASE]-?STRING"
  (let* ((#1=array-dimensions (#1# obj))
         (#2=array-element-type (#2# obj))
         (#3=adjustable-array-p (#3# obj))
         (#4=array-has-fill-pointer-p (#4# obj))
	 (#5=fill-pointer (when #4# (#5# obj))))
    (loop
       :with new-vector = (make-array array-dimensions
                                      :element-type array-element-type
                                      :adjustable adjustable-array-p
                                      :fill-pointer fill-pointer)
       :for cell-data :across obj
       :for cell-int :from 0 :to (if array-has-fill-pointer-p
                                     fill-pointer
                                     (first array-dimensions))
       :do (setf (aref new-vector
                       cell-int)
                 (if (= 0 depth)
                     cell-data
                     (copy-depth-rec cell-data
                                     (1- depth))))
       :finally (return new-vector))))

(defmethod copy-depth-rec ((obj hash-table) (depth integer))
  "(Mutable) Hash Tables: HASH-TABLE"
  (loop
     :with new-hash-table = (make-hash-table :test (hash-table-test obj)
                                             :size (hash-table-size obj)
                                             :rehash-size (hash-table-rehash-size obj)
                                             :rehash-threshold (hash-table-rehash-threshold obj))
     :for hash-key :being :the :hash-keys :of obj :using (:hash-value hash-value)
     :do (setf (gethash (if (= 0 depth)
                            hash-key
                            (copy-depth-rec hash-key
                                            (1- depth)))
                        new-hash-table)
               (if (= 0 depth)
                   hash-value
                   (copy-depth-rec hash-value
                                   (1- depth))))
     :finally (return new-hash-table)))

(defmethod copy-depth-rec ((obj symbol) (depth integer))
  "(Immutable) Symbols: SYMBOL, NULL (reduce #'eq '() () nil 'nil), KEYWORD, 
BOOLEAN"
  obj)

(defmethod copy-depth-rec ((obj number) (depth integer))
  "(Immutable) Numbers: NUMBER, COMPLEX, REAL, [SHORT|SINGLE|DOUBLE|LONG]-?FLOAT, 
RATIONAL, RATIO, INTEGER, [FIX|BIG]NUM, [SIGNED|UNSIGNED]-BYTE, BIT"
  obj)

(defmethod copy-depth-rec ((obj character) (depth integer))
  "(Immutable) Characters: CHARACTER, [EXTENDED|BASE|STANDARD]-CHAR"
  obj)

#| ------- Misc -------------------------------------------------------------- |#
(defgeneric file-contents->string (filename)
  (:documentation "Function to read file contents and return as a single string.")
  (:method ((filename pathname))
    (with-open-file (stream filename)
      (let ((contents (make-string (file-length stream))))
        (read-sequence contents stream)
        contents))))

(defgeneric file-lines->list-of-strings (filename)
  (:documentation "Function to read file contents and return lines as list of 
strings.")
  (:method ((filename pathname))
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil)
         while line
         collect line))))

(defgeneric vector->list-indices-nil/t (vector)
  (:documentation "Collect unpopulated and populated vector cell indices and 
return as respective lists.")
  (:method ((v vector))
    (loop
       :for x :across v
       :for i :from 0
       :if x :collect i :into a
       :else :collect i :into b
       :finally (return (values b a)))))

(defgeneric char-interval->list (char1 char2)
  (:documentation "Return list of characters on interval between the char1 and 
char2 character codes inclusive.")
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
  
(defgeneric symbol->list-in-macro (object)
  (:documentation "Take unevaluated object that is either a list or symbol and 
make sure it is in-list-ed."))

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

(defmacro do-hash-keys ((var table &optional result) &body body)
  "Iterate over the keys of a hash-table in similar fashion to dolist."
  `(progn
     (alexandria:maphash-keys #'(lambda (,var)
                                  ,@body)
                              ,table)
     ,result))
