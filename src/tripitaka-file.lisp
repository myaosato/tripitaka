(in-package :tripitaka)

;;; VARIABLE
(defvar *file-cache* (make-hash-table :test 'equal))
(defvar *dat-dir*)
(defvar *html-dir*)
(defvar *sync-file*)

;;; PATH
(defun get-data-path (name)
  (merge-pathnames (format nil "~A.rosa" name) *dat-dir*))

(defun get-html-path (name)
  (merge-pathnames (format nil "~A.html" name) *home-dir*))

;;; DATA
(defun file->data (path)
  (with-open-file (in path)
    (rosa:peruse-as-plist in #'string-upcase)))

(defun name->data (name)
  (let ((date (gethash name)))
    (if data
        data
        (file->data (get-data-path name)))))

(defun data->file (data path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (princ (rosa:indite data) out)))

(defun data->file-by-name (data name)
  (with-open-file (out (name->file name) :direction :output :if-exists :supersede)
    (princ (rosa:indite data) out)))

(defun get-prop-from-data (data prop &optional (type 'string))
  (cond ((eql type 'list) (coerce (getf data prop) 'list))
        ((eql type 'vector) (getf data prop))
        (t (concatenate 'string (getf data prop)))))

(defun get-prop-as-list (data prop)
  (get-prop data prop 'list))

(defun get-prop (name prop &optional (type 'string))
  (get-prop (name->data name) type))

(defun get-prop-as-list (name prop &optional (type 'string))
  (get-prop name prop 'list))

(defun set-prop (data prop value)
  (cond ((listp value) (setf (getf data prop) (coerce value 'vector)))
        (t (setf (getf data prop) #((princ-to-string value))))))

;;; FILE-LIST  
(defun dir-p (pathname)
  (and (not (pathname-name pathname))
       (not (pathname-type pathname))))

(defun get-write-time (pathname)
  (local-time:format-timestring 
   nil 
   (local-time:universal-to-timestamp (file-write-date pathname)) 
   :format '(:year (:month 2) (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2))))

(defun get-type-or-dir (pathname)
  (if (dir-p pathname)
      'dir
      (pathname-type pathname)))

(defun get-file-name (pathname)
  (if (dir-p pathname)
      (format nil "~A/" (car (last (pathname-directory pathname))))
      (format nil 
              "~:[~;~:*~A~]~:[~;.~:*~A~]" 
              (pathname-name pathname) 
              (pathname-type pathname))))

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


;;; MANAGE 
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
          (format nil "~A>~A" data-timestamp html-timestamp))
    (sava-hashtable-as-rosa-file sync-hash *sync-file*)))










