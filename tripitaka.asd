(in-package :cl-user)

(defpackage tripitaka-asd
  (:use :cl :asdf))

(in-package :tripitaka-asd)

(defsystem tripitaka
  :name "tripitaka"
  :version "1.0.0"
  :author "Satoaki Miyao"
  :licence "MIT"
  :description "static site generator"
  :long-description "Tripitaka is a simple static site generator. It make html file from template files and simple structured text files."
  :components ((:module "src"
                :serial t
                :components ((:file "tripitaka"))))
  :depends-on (:cl-fad :cl-ppcre :local-time :cl-markdown :alexandria :rosa))

