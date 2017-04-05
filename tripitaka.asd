(in-package :cl-user)

(defpackage tripitaka-asd
  (:use :cl :asdf))

(in-package :tripitaka-asd)

(defsystem tripitaka
  :name "tripitaka"
  :version "0.0.1"
  :author "Satoaki Miyao"
  :licence "MIT"
  :description "Static Site Maneger"
  :long-description "Static Site Maneger includes html file generator and simple atom feed generator and very simple ftp client"
  :components ((:module "src"
                :serial t
                :components ((:file "tripitaka-package")
                             (:file "tripitaka-ftp")
                             (:file "tripitaka-atom")
                             (:file "tripitaka" :depends-on ("tripitaka-atom")))))
  :depends-on (:cl-fad :cl-ppcre :local-time :cl-markdown :usocket :cl-ftp))

