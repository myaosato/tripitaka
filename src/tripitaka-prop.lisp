(in-package :tripitaka)

(defvar *file-cache* (make-hash-table :test 'equal))

(defun name->file (name)
  (merge-pathnames *data-dir* (concatenate 'string name ".rosa")))

(defun file->data (path)
  (with-open-file (in path)
    (rosa:peruse-as-plist in #'string-upcase)))

(defun name->data (name)
  (let ((date (gethash name)))
    (if data
        data
        (file->dat (name->file name)))))
    
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





