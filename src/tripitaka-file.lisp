(in-package :tripitaka)


(defvar *dat-dir*)
(defvar *html-dir*)
(defvar *sync-file*)

(defun dir-p (pathname)
  (and (not (pathname-name pathname))
       (not (pathname-type pathname))))

(defun get-file-name (pathname)
  (if (dir-p pathname)
      (format nil "~A/" (car (last (pathname-directory pathname))))
      (format nil 
              "~:[~;~:*~A~]~:[~;.~:*~A~]" 
              (pathname-name pathname) 
              (pathname-type pathname))))

(defun get-write-time (pathname)
  (local-time:format-timestring 
   nil 
   (local-time:universal-to-timestamp (file-write-date pathname)) 
   :format '(:year (:month 2) (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2))))
      
(defun get-type-or-dir (pathname)
  (if (dir-p pathname)
      'dir
      (pathname-type pathname)))

(defun get-children-list (dir)
  (mapcar #'(lambda (pathname) (list (get-file-name pathname)
                                     (get-write-time pathname)
                                     (get-type-or-dir pathname)))
          (cl-fad:list-directory (cl-fad:pathname-as-directory dir))))

(defun get-files (dir extension)
  (let ((all-list (get-children-list dir)))
    (append 
     (remove-if-not (lambda (elt) (equal (nth 2 elt) extension)) all-list))))
  
(defun get-data-files ()
  (get-files *dat-dir* "rosa"))

(defun get-html-files ()
  (get-files *html-dir* "html"))

(defun get-rosa-file-as-hashtable (file)
  (with-open-file (in file)
    (rosa:peruse in)))

(defun sava-hashtable-as-rosa-file (hashtable file)
  (with-open-file (out file :direction :output :if-exists :supersede)
    (rosa:indite hashtable)))

(defun string-to-symbol (str)
  (eval (read-from-string (format nil "'~A" str))))
  
(defun registor-convert-time (name data-timestamp html-timestamp)
  (let ((sync-hash (get-rosa-file-as-hashtable *sync-file*)))
    (setf (gethash (string-to-symbol name) sync-hash)
          (format nil "~A:~A" data-timestamp html-timestamp))
    (sava-hashtable-as-rosa-file sync-hash *sync-file*)))










