;;;;
;;;;
;;;;

;;; Package
(eval-when (:load-toplevel :compile-toplevel)
  (ql:quickload :cl-fad :silent t)
  (ql:quickload :cl-ppcre :silent t)
  (ql:quickload :local-time :silent t)
  (ql:quickload :cl-markdown :silent t)
  (load "tripitaka-atom"))
(defpackage :tripitaka-html
  (:use :common-lisp)
  (:export :read-rc
           :set-project
           :file2plist
           :make-new-project
           :make-new-file
           :make-html)
(in-package :tripitaka-html)

;;; Define special valiable
;;;
(defvar *rc-file* (merge-pathnames #p".tripitakarc"
                                          (user-homedir-pathname)))
(defvar *projects-plist* nil)
(defvar *project-dir*
  (cl-fad:pathname-as-directory "~/default-project/"))
(defvar *home-dir* "")
(defvar *dat-dir* "")
(defvar *project-file* "")
(defvar *project* nil)
(defvar *site-url* "")
(defvar *site-name* "")

;;; Utils
;;;
(defun read-sym-str-file (file)
  (with-open-file (in file)
    (do ((plist nil)
         (sym (read in nil nil) (read in nil nil)))
        ((not sym) plist)
      (setf (getf plist sym)
            (read in nil nil)))))

(defun get-date-string (arg)
  "get date as string YYYYMMDD separated by arg"
  (local-time:format-timestring nil (local-time:now)
                                :format (list '(:year 4) arg '(:month 2) arg '(:day 2))))

(defun file2string (filespec)
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

;;; System
;;;
(defun message (text)
  (format t "TRIPITAKA: ~A" text))

;;; Initialize
;;;
(defun read-rc ()
  "read .tripitakarc"
  (setf *projects-plist* (read-sym-str-file *rc-file*)))

(defun load-project ()
  "read config file"
  (setf *project* (read-sym-str-file *project-file*)))

(defun set-project (project)
  "set project config"
  (setf *project-dir*
        (cl-fad:pathname-as-directory (getf *projects-plist* (string-to-keyword project))))
  (setf *project-file* (merge-pathnames ".project" *project-dir*))
  (setf *home-dir* (merge-pathnames "home/" *project-dir*))
  (setf *dat-dir* (merge-pathnames "dat/" *project-dir*))
  (load-project))

(defun project (name)
  "get value from config"
  (getf *project* (string-to-keyword name)))

;;; Utils for tripitaka
;;;
(defun find-dat (name)
  "if name.dat dosen't exist return nil"
  (probe-file (merge-pathnames (format nil "~A.dat" name) *dat-dir*)))

(defun find-text-keyword (line)
  (cl-ppcre:scan "^:TEXT" (string-trim " " (string-upcase line))))

;;; Make plist from file 
;;;
(defun md-to-html-string (target)
  (nth 1 (multiple-value-list (cl-markdown:markdown target :stream nil))))

(defun file2plist (name)
  "make plist as article from file."
  (let ((plist nil) sym val)
    (with-open-file (in (merge-pathnames (format nil "~A.dat" name) *dat-dir*))
      (do ((line (read-line in nil nil) (read-line in nil nil)))
          ((or (find-text-keyword line) (not line)) plist)
        (with-open-stream (in-line (make-string-input-stream line))
          (setf sym (read in-line nil nil))
          (setf val (read in-line nil nil))
          (setf (getf plist sym) (if val val ""))))
      (do ((result "" (format nil "~a~&~a" result line))
           (line (read-line in nil nil) (read-line in nil nil)))
          ((not line)
           (setf (getf plist :text) (md-to-html-string result))))
    plist)))

;;; Make new files
;;;
(defun make-new-project (name)
  (let* ((pdir (getf *project* name))
         (conf-file (merge-pathnames ".project" pdir)))
    (with-open-file (out conf-file :direction :output :if-exists :supersede)
      (format out ":site-name \"\"~%")
      (format out ":site-url \"\"~%")
      (format out ":author \"\"~%")
      (format out ":pubyear \"\"~%"))))

(defun make-new-file (name &key
                             (title "")
                             (date (get-date-string "-"))
                             (up "")
                             (next "")
                             (prev "")
                             (uuid "")
                             (text "")
                             (overwrite nil))
  (let* ((path (merge-pathnames (format nil "~A.dat" name) *dat-dir*)))
    (if (and (not overwrite) (probe-file path))
        (message (format nil "name \"~A\" has already been used!" name))
        (with-open-file (out path
                             :direction :output
                             :if-exists :supersede)
          (format out ":TITLE \"~A\"~%" title)
          (format out ":DATE \"~A\"~%" date)
          (format out ":UP \"~A\"~%" up)
          (format out ":PREV \"~A\"~%" prev)
          (format out ":NEXT \"~A\"~%" next)
          (format out ":UUID \"~A\"~%" uuid)
          (format out ":TEXT~%")
          (format out "~A" text)))))

;;; make HTML String
;;;
;;;
(defun scan-templete (target start-pos plist &optional (count 0))
  (let (type prop n name func result (attr (make-hash-table :test #'equal)))
    (multiple-value-bind (start end svect evect) 
        (cl-ppcre:scan "<tri:([^\\s]+)(?:\\s+([A-Za-z]+)=\"([^\\s\"]+|)\")+\\s*>" target :start start-pos)
      (cond (start 
             (setf type (subseq target (aref svect 0) (aref evect 0)))
             (do* ((lim (length svect))
                   (ind 1 (+ ind 2))
                   (key-s (aref svect ind))
                   (key-e (aref evect ind))
                   (val-s (aref svect (1+ ind)))
                   (val-e (aref evect (1+ ind))))
                  ((>= ind lim) nil)
               (setf (gethash (subseq target key-s key-e) attr) (subseq target val-s val-e)))
             (setf prop (gethash "prop" attr))
             (setf n (gethash "nth" attr))
             (setf name (gethash "name" attr))
             (setf func (cond ((String= type "project")
                               #'tripitaka::project)
                              ((String= type "page")
                               (cond ((not name)
                                      #'(lambda (prop) (getf plist (string-to-keyword prop))))
                                     ((find-dat name)
                                      #'(lambda (prop) (getf (file2plist name) (string-to-keyword prop))))
                                     (t #'any-to-blank)))
                              (t #'any-to-blank)))
             (setf result (if n (nth (parse-integer n) (funcall func prop)) (funcall func prop)))
             (scan-templete (format nil "~A~A~A" (subseq target 0 start) result (subseq target end))
                            (+ start (length result))
                            plist
                            (1+ count)))
            ((= count 0)
              target)
             (t
              (scan-templete target
                             0
                             plist))))))

(defun make-from-templete (name)
  (let ((target (file2string (merge-pathnames "templete" *project-dir*))))
    (scan-templete target
                   0
                   (file2plist name))))

(defun make-html (name)
  "make html file"
  (if (find-dat name)
      (with-open-file (out (merge-pathnames (format nil "~A.htm" name) *home-dir*)
                           :direction :output
                           :if-exists :supersede)
        (format out "~A" (make-from-templete name)))
      (message (format nil "name \"~A\" can't be found!" name))))

