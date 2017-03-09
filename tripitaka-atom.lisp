;;;; Make Atom feed utils
;;;; Recently this is test(developping) version
;;;;
(eval-when (:load-toplevel :compile-toplevel)
  (ql:quickload :local-time :silent t)
  (ql:quickload :cl-markup :silent t)
  (ql:quickload :uuid :silent t))
(defpackage :tripitak-atom
  (:use :common-lisp)
  (:export))

(in-package :tripitaka-atom)
(defvar *feed-dat* "")

(defun gen-uuid ()
  (let ()
    (uuid:format-as-urn nil (uuid:make-v1-uuid))))

(defun atom-date-now ()
  (local-time:format-timestring nil (local-time:now)))


(defun atom-entries ()
  (with-open-file (in *feed-dat*)
    (do (result
         (elt (read in nil nil)
              (read in nil nil)))
        ((not elt) (nreverse result))
      (push elt result))))

(defun make-entry-list (&key
                          (title "TITLE")
                          (link "LINK")
                          (uuid (gen-uuid))
                          (date (atom-date-now))
                          (summary
                           "This article has been updated"))
  (append `((:entry
             #\Newline
             (:title ,title) #\Newline
             (:link :href ,link) #\Newline
             (:id ,uuid) #\Newline
             (:update ,date) #\Newline
             (:summary ,summary) #\Newline) #\Newline)
          (atom-entries)))

(defmacro parse-markup-list (markup-list)
  `(cl-markup:markup ,(eval markup-list)))
 
(defmacro update-atom-feed (&key
                              (title "TITLE")
                              (link "LINK")
                              (uuid (gen-uuid))
                              (date (atom-date-now))
                              (summary
                               "This article has been updated"))
  (let ((entry-list (make-entry-list :title title
                                     :link link
                                     :uuid uuid
                                     :date date
                                     :summary summary)))
  `(progn
     (with-open-file (out ,(merge-pathnames *feed* *home-dir*)
                          :direction :output
                          :if-exists :supersede)
       (format out "~a~%~a"
               ,(cl-markup:doctype :xml)
               (parse-markup-list
                (make-atom-feed-list
                 ',entry-list))))
     (with-open-file (out ,*feed-dat*
                          :direction :output
                          :if-exists :supersede)
       (format out "~a" 
               (do* ((result "")
                     (list ',entry-list
                           (rest list))
                     (elt (car list) (car list)))
                    ((not elt) result)
                 (setf result
                       (format nil "~a~&~a" 
                               result
                               (write-to-string elt)))))))))
