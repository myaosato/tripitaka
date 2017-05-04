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

