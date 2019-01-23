;;;; package.lisp

(defpackage #:org.unaen.cl.util
  (:use #:cl)
  (:export :inst->slots-list
           :with-dot-slots
           :copy-all
           :copy-depth
           :file-contents->string
           :file-lines->list-of-strings
           :vector->list-indices-nil/t
           :char-interval->list
           :list->pairs
           :separate-if
           :symbol->list-in-macro
           :plusplus
           :truth
           :false
           :alias
           :do-hash-keys))
