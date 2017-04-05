(in-package :cl-user)
(defpackage :tripitaka
  (:use :common-lisp)
  (:export :mirroring-upload
           :gen-id-uuid-v4
           :gen-id-tag-uri
           :iso8601-time
           :update-atom
           :ready
           :read-rc
           :set-project
           :make-new-project
           :make-dat
           :dat-to-plist
           :make-html
           :make-new-diary
           :diary-update))

