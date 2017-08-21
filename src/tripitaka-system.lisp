(in-package :tripitaka)

(defvar *rc-file* (merge-pathnames #p".tripitakarc"
                                          (user-homedir-pathname)))
(defvar *projects-plist* nil)
(defvar *project-dir* nil)
(defvar *home-dir* nil)
(defvar *data-dir* nil)
(defvar *theme-dir* nil)
(defvar *project-file* nil)
(defvar *project* nil)
(defvar *feed-dat* nil)
(defvar *feed-atom* nil)

(defvar *gen-id* (lambda () nil))

(defvar *no-file-err* "Such file is not found.")

(defun message (text &optional (result nil))
  (format t "~&TRIPITAKA> ~A~%" text)
  result)

(defmacro if-file-exists-do ((filespec) &body body)
  `(if (probe-file filespec)
       (progn ,@body)
       (progn
         (message (format nil "~A ~A" *no-file-err* filespec))
         nil)))
       
(defun get-data-path (name)
  (merge-pathnames (format nil "~A.rosa" name) *dat-dir*))

(defun get-html-path (name)
  (merge-pathnames (format nil "~A.html" name) *home-dir*))

(defun get-date-string (arg)
  "get date as string YYYYMMDD separated by arg"
  (local-time:format-timestring nil (local-time:now)
                                :format (list '(:year 4) arg '(:month 2) arg '(:day 2))))

(defun file->string (filespec)
  (with-open-file (in filespec)
    (do ((result "")
         (line (read-line in nil nil) (read-line in nil nil)))
        ((not line) result)
      (setf result (format nil "~A~&~A" result line)))))

(defun string-to-keyword (string)
  (read-from-string (format nil ":~A" string)))

(defun any-to-blank (&rest any)
  (declare (ignore any)) 
  "")

(defun make-dir (dir-path)
  (second (multiple-value-list (ensure-directories-exist
                                (cl-fad:pathname-as-directory dir-path)))))

(defun md-to-html-string (target)
  (nth 1 (multiple-value-list (cl-markdown:markdown target :stream nil))))
