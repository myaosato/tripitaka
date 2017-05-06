(in-package :cl-user)
(defpackage :tripitaka
  (:use :common-lisp)
  (:export :ready
           :read-rc
           :set-project
           :make-project
           :make-dat
           :make-html
           :update-page
           :make-new-diary
           :update-diary
           :update-diary-with-feed
           :update-default-feed
           :update-atom))
(in-package :tripitaka)
(defvar *charset-utf8*
  #+(or sbcl ccl cmu allegro ecl lispworks) :utf-8
  #+clisp charset:utf-8)

