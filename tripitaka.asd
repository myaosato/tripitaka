(in-package :cl-user)

(defpackage tripitaka-asd
  (:use :cl :asdf))

(in-package :tripitaka-asd)

(defsystem tripitaka
  :name "tripitaka"
  :version "0.1.0"
  :author "Satoaki Miyao"
  :licence "MIT"
  :description "Static Site Maneger"
  :long-description "Static Site Maneger includes html file generator and simple atom feed generator and very simple ftp client"
  :components ((:module "src"
                :serial t
                :components ((:file "tripitaka-package")
                             (:file "tripitaka-ftp")
                             (:file "tripitaka-atom")
                             (:file "tripitaka-system")
                             (:file "tripitaka"
                                    :depends-on ("tripitaka-atom" "tripitaka-system"))
                             (:file "tripitaka-update" :depends-on ("tripitaka")))))
  :depends-on (:cl-fad :cl-ppcre :local-time :cl-markdown :usocket :cl-ftp))

