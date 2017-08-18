(in-package :tripitaka)

(defun name->file (name)
  (merge-pathnames *dat-dir* (concatenate 'string name ".dat")))

(defun file->data (path)
  (with-open-file (in path)
    (rosa:peruse-as-plist in #'string-upcase)))

(defun name->data (name)
  (file->dat (name->file name)))
    
(defun data->file (data path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (princ (rosa:indite data) out)))

(defun data->file-by-name (name file)
  (with-open-file (out (name->file name) :direction :output :if-exists :supersede)
    (princ (rosa:indite data) out)))

(defun get-prop (data prop &optional (type 'string))
  (cond ((eql type 'list) (coerce (getf data prop) 'list))
        ((eql type 'vector) (getf data prop))
        (t (concatenate 'string (getf data prop)))))

(defun get-prop-as-list (data prop)
  (get-prop data prop 'list))

(defun get-prop-by-name (name prop &optional (type 'string))
  (get-prop (name->data name) type))

(defun get-prop-by-name-as-list (name prop &optional (type 'string))
  (get-prop-by-name name prop 'list))

(defun set-prop (data prop value)
  (cond ((listp value) (setf (getf data prop) (coerce value 'vector)))
        (t (setf (getf data prop) #((princ-to-string value))))))





