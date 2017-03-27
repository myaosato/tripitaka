;;;; Make Atom feed utils
;;;; Recently this is beta version
;;;;
(eval-when (:load-toplevel :compile-toplevel)
  (ql:quickload :local-time :silent t)
  (ql:quickload :cl-markup :silent t)
  (ql:quickload :uuid :silent t))
(defpackage :tripitaka-atom
  (:use :common-lisp)
  (:export :gen-uuid))

(in-package :tripitaka-atom)

(defun gen-uuid ()
  (let ()
    (uuid:format-as-urn nil (uuid:make-v1-uuid))))

(defun iso8601-time ()
  (local-time:format-timestring nil (local-time:now)))




