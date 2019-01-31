;;;; org.unaen.cl.util/copy.lisp

(uiop:define-package #:org.unaen.cl.util/copy
    (:use #:common-lisp)
    (:export #:copy-all
             #:copy-depth))

(in-package #:org.unaen.cl.util/copy)

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
