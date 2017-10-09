(in-package :cl-user)

(defpackage :tripitaka
  (:use :common-lisp)
  (:export ))

(in-package :tripitaka)

(defvar *charset-utf8*
  #+(or sbcl ccl cmu allegro ecl lispworks) :utf-8
  #+clisp charset:utf-8)

