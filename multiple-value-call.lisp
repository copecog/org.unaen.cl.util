;;;; org.unaen.cl.util/multiple-value-call.lisp

(uiop:define-package #:org.unaen.cl.util/multiple-value-call
    (:use #:common-lisp)
    (:export #:set-dispatch-macro-V
             #:set-dispatch-macro-⸨))

(in-package #:org.unaen.cl.util/multiple-value-call)

(defun |#V-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((func-exp (read stream t nil t)))
    `(multiple-value-call #',(car func-exp) ,@(cdr func-exp))))

(defun set-dispatch-macro-V ()
  (set-dispatch-macro-character #\# #\V #'|#V-reader|))
  
;;; Rest via https://gist.github.com/informatimago (thanks!)
(defun |⸨mvc⸩| (stream char)
  (declare (ignore char))
  (let ((call (read-delimited-list #\⸩ stream t)))
    `(multiple-value-call (function ,(first call)) ,@(rest call))))

(defun set-dispatch-macro-⸨ ()
  (set-macro-character #\⸨ (function |⸨mvc⸩|)))

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

     
