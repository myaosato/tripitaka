;;;; Make Atom feed utils
;;;; 
;;;;
(in-package :tripitaka)


;;; Utils for Atom feed
;;;
(defun gen-id-tag-uri (&key authority specific fragment)
  (let ((date (local-time:format-timestring nil (local-time:now)
                                            :format (list '(:year 4) #\- '(:month 2) #\- '(:day 2)))))
        (format nil "tag:~A,~A:~A#~A" authority date specific fragment)))

(defun random-hex (num &optional (seq ""))
  (if (> num 1)
      (random-hex (1- num) (format nil "~A~(~X~)" seq (random 16)))
      (format nil "~A~(~X~)" seq (random 16))))


(defun gen-id-uuid-v4 ()
  (format nil "urn:uuid:~A-~A-4~A-~A~A-~A"
          (random-hex 8)
          (random-hex 4)
          (random-hex 3)
          (format nil "~(~X~)" (+ 8 (random 4)))
          (random-hex 3)
          (random-hex 12)))

(defun iso8601-time ()
  (local-time:format-timestring nil (local-time:now)))

;;; make elements
;;;
(defun make-entry (&key (title "") (link "") (id "") (updated "") (summary ""))
  (list :title title
        :link link
        :id id
        :updated updated
        :summary summary))

(defun make-feed (&key (title (project "site-name"))
                    (link (project "site-url"))
                    (id (gen-id-uuid-v4))
                    (updated (iso8601-time))
                    (author (project "author")) 
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
     collect (cons k v)))

(defun list->entries (entry-list)
  (let ((hash (make-hash-table :test #'equal)))
    (loop for k-v in entry-list
       do (setf (gethash (car k-v) hash) (cdr k-v)))
    hash))

(defun feed4save (feed)
  (list :title (getf feed :title)
        :link (getf feed :link)
        :id (getf feed :id)
        :updated (getf feed :updated)
        :author (getf feed :author)
        :entries (entries->list (getf feed :entries))))

(defun save-feed (feed filepath)
  (with-open-file (out filepath
                       :direction :output
                       :if-exists :supersede)
    (print (feed4save feed) out)))

(defun load-feed-helper (feed)
  (list :title (getf feed :title)
        :link (getf feed :link)
        :id (getf feed :id)
        :updated (getf feed :updated)
        :author (getf feed :author)
        :entries (list->entries (getf feed :entries))))

(defun load-feed (filepath)
  (if (probe-file filepath)
      (with-open-file (in filepath)
        (with-standard-io-syntax
          (load-feed-helper (read in))))
      (make-feed)))

;;;
;;;
(defun entry-string (entry)
  (concatenate 'string
               (format nil "<entry>~%")
               (format nil "<title>~A</title>~%" (getf entry :title))
               (format nil "<link href=\"~A\"/>~%" (getf entry :link))
               (format nil "<id>~A</id>~%" (getf entry :id))
               (format nil "<updated>~A</updated>~%" (getf entry :updated))
               (format nil "<summary>~A</summary>~%" (getf entry :summary))
               (format nil "</entry>~%")))

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
               (format nil "<id>~A</id>~%" (getf feed :id))
               (format nil "<updated>~A</updated>~%" (getf feed :updated))
               (format nil "<author>~%<name>~A</name>~%</author>~%" (getf feed :author))
               (entries-string (getf feed :entries))
               (format nil "</feed>")))

(defun write-feed (feed filepath)
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

(defun update-atom (dat file &key (title "") (link "") (id "") (updated (iso8601-time)) (summary ""))  
  (let ((feed (load-feed dat))
        (entry (make-entry :title title
                           :link link
                           :id id
                           :updated updated
                           :summary summary)))
    (update-entry entry feed updated)
    (write-feed feed file)
    (save-feed feed dat)))

(defun get-feed-id ()
  (getf (load-feed) :id))
