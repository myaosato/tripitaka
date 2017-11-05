(in-package :cl-user)

(defpackage tripitaka-asd
  (:use :cl :asdf))

(in-package :tripitaka-asd)

(defsystem tripitaka
  :name "tripitaka"
  :version "1.0.0"
  :author "Satoaki Miyao"
  :licence "MIT"
  :description "Static Site Maneger"
  :long-description "Static Site Maneger includes html file generator and simple atom feed genertor and file manager for website"
  :components ((:module "src"
                :serial t
                :components ((:file "tripitaka"))))
  :depends-on (:cl-fad :cl-ppcre :local-time :cl-markdown :alexandria :rosa))

