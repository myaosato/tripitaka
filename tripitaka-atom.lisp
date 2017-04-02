;;;; Make Atom feed utils
;;;; Recently this is beta version
;;;;
(eval-when (:load-toplevel :compile-toplevel)
  (ql:quickload :local-time :silent t)
  (ql:quickload :cl-markup :silent t)
  (ql:quickload :uuid :silent t))
(defpackage :tripitaka-atom
  (:use :common-lisp)
  (:export :gen-uuid
           :iso8601-time
           :update))

(in-package :tripitaka-atom)

;;; Utils for Atom feed
;;;
(defun gen-uuid ()
  (let ()
    (uuid:format-as-urn nil (uuid:make-v1-uuid))))

(defun iso8601-time ()
  (local-time:format-timestring nil (local-time:now)))

;;; make elements
;;;
(defun make-entriy (&key (title "") (link "") (id "") (updated "") (summary ""))
  (list :title title
        :link link
        :id id
        :updated updated
        :summary summary))

(defun make-feed (&key (title "") (link "") (id "") (updated "") (author "") 
                    (entries (make-hash-table :test #'equal)))
  (list :title title
        :link link
        :id id
        :updated updated
        :author author
        :entries entries))

;;; save atom feed as plist for tripitaka  
;;;
(defun entries->list (entry-hash)
  (loop for k being the hash-keys in entry-hash using (hash-value v)
     collect (cons k (entry-string v))))

(defun list->entries (entry-list)
  (let ((hash (make-hash-table :test #'equal)))
    (loop for k-v in entry-list
       do (setf (gethash (car k-v) hash) (cdr k-v)))))

(defun feed4save (feed)
  (list :title (getf feed :title)
        :link (getf feed :link)
        :id (getf feed :id)
        :updated (getf feed :update)
        :author (getf feed :author)
        :entries (entries->list (getf feed :entries))))

(defun save-feed (feed filepath)
  (with-open-file (out filepath
                       :direction :output
                       :if-exists :supersede)
    (print out (feed4save feed))))

(defun load-feed-helper (feed)
  (list :title (getf feed :title)
        :link (getf feed :link)
        :id (getf feed :id)
        :updated (getf feed :update)
        :author (getf feed :author)
        :entries (list->entries (getf feed :entries))))

(defun load-feed (filepath)
  (with-open-file (in filepath)
    (with-standard-io-syntax
      (load-feed-helper (read in)))))

;;;
;;;
(defun enrty-string (entry)
  (concatenate 'string
               (format nil "<entry>~%")
               (format nil "<title>~A</title>~%" (getf entry :title))
               (format nil "<link href=\"~A\"/>~%" (getf entry :link))
               (formar nil "<id>~A</id>~%" (getf entry :id))
               (format nil "<updated>~A</updated>~%" (getf entry :updated))
               (format nil "<summary>~A</summary>~%" (getf entry :summary))
               (fromat nil "</entry>~%")))

(defun entries-string (entry-hash)
  (apply #'concatenate 'string
         (loop for k being the hash-keys in entry-hash using (hash-value v)
            collect (entry-string v))))

(defun feed-string (feed)
  (concatenate 'string
               (format nil "<?xml version=\"1.0\" encoding=\"utf-8\"?>~%")
               (format nil "<feed xmlns=\"http://www.w3.org/2005/Atom\">~%")
               (format nil "<title>~A</title>~%" (getf feed :title))
               (format nil "<link href=\"~A\"/>~%" (getf feed :link))
               (formar nil "<id>~A</id>~%" (getf feed :id))
               (format nil "<updated>~A</updated>~%" (getf feed :updated))
               (format nil "<author>~%<name>~A</name>~%</auhtor>~%" (getf feed :updated))
               (entries-string (getf feed :entries))
               (format nil "</feed>")))

(defun write-feed (feed file)
  (with-open-file (out filepath
                       :direction :output
                       :if-exists :supersede)
    (format out "~A" (feed-string feed))))

;;;
;;;
(defun add-entry (entry feed)
  (setf (gethash (getf entry :id) (getf feed :entries)) entry)
  feed)

(defun update-entry (entry feed updated)
  (setf (getf entry :updated) updated)
  (setf (getf feed :updated) updated)
  (add-entry entry feed))

(defun update (dat file &key (title "") (link "") (id "") (updated (iso8601-time)) (summary ""))  
  (let ((feed (feed-load dat))
        (entry (make-feed :title title
                          :link link
                          :id id
                          :updated updated
                          :summary summary)))
    (update-entry entry feed updated)
    (write-feed feed file)
    (save-feed feed dat)))

