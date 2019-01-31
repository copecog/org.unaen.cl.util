;;;; org.unaen.cl.util/file-contents.lisp

(uiop:define-package #:org.unaen.cl.util/file-contents
    (:use #:common-lisp)
    (:export #:file-contents->string
             #:file-lines->list-of-strings))

(in-package #:org.unaen.cl.util/file-contents)

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
