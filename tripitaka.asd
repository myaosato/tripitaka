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
  :long-description "Static Site Maneger includes html file generator and simple atom feed generator and very simple ftp client"
  :components ((:module "src"
                :serial t
                :components ((:file "tripitaka-package")
                             (:file "tripitaka-file"
                              :depends-on ("tripitaka-package"))
                             (:file "tripitaka-prop"
                              :depends-on ("tripitaka-file"))
                             (:file "tripitaka-converter"
                              :depends-on ("tripitaka-prop"))
                             (:file "tripitaka-functions" 
                              :depends-on ("tripitaka-converter")))))
  :depends-on (:cl-fad :cl-ppcre :local-time :cl-markdown :usocket :cl-ftp :rosa))

