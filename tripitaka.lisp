;;;;
;;;;
;;;;

;;; Package
(eval-when (:load-toplevel :compile-toplevel)
  (ql:quickload :cl-fad :silent t)
  (ql:quickload :cl-ppcre :silent t)
  (ql:quickload :local-time :silent t)
  (ql:quickload :uuid :silent t)
  (load "tripitaka-atom"))
(defpackage :tripitaka
  (:use :common-lisp)
  (:use :tripitaka-atom)
  (:export ))
(in-package :tripitaka)

;;; Define special valiable
;;;
(defvar *rc-file* (merge-pathnames #p".tripitakarc"
                                          (user-homedir-pathname)))
(defvar *projects-plist* (make-hash-table :test #'equal))
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
(defun read-sym-str-file (file plist)
  (with-open-file (in file)
    (do ((sym (read in nil nil) (read in nil nil)))
        ((not sym) plist)
      (setf (getf sym plist)
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

;;; System
;;;
(defun message (text)
  (format t "TRIPITAKA: ~A" text))

;;; Initialize
;;;
(defun read-rc ()
  "read .tripitakarc"
  (read-sym-str-file *rc-file* *project*))

(defun load-project ()
  "read config file"
  (read-sym-str-file *project-file* *project*))

(defun project (name)
  "get value from config"
  (gethash name *project*))

(defun set-project (project)
  "set project config"
  (setf *project-dir* (cl-fad:pathname-as-directory (project project)))
  (setf *project-file* (merge-pathnames ".project" *project-dir*))
  (setf *home-dir* (merge-pathnames "home/" *project-dir*))
  (setf *dat-dir* (merge-pathnames "dat/" *project-dir*))
  (load-project))

;;; Make plist from file 
;;;
(defun file2plist (name)
  "make plist as article from file."
  (let ((plist nil))
    (with-open-file (in (merge-pathnames (format nil "~A.dat" name) *dat-dir*))
      (do ((sym (read in nil nil) (read in nil nil)))
          ((or (eq sym :text) (not sym)) plist)
        (setf (getf sym plist) (read in nil nil)))
      (do ((result "" (format nil "~a~%~a" result line))
           (line (read-line in nil nil) (read-line in nil nil)))
          ((not line) (setf (getf plist :text) result))))
    plist))

;;; make HTML String
;;;
;;;
(defun scan-templete (file start-pos) 
  (let ((target (file2string file)))
    (multiple-value-bind (start end svect evect) 
        (cl-ppcre:scan "<tri:([^\\s]+) prop=\"([^\\s]+)\"(?: nth=\"(\\d+)\")?>" target :start start-pos)
      (append (list start end)
              (list (subseq target start end))
              (loop for s across svect
                 for e across evect
                 collect (if (and s e) (subseq target s e) nil))))))


(defun scan-templete-cycle (target start-pos plist)
  (let (type prop n name func result (attr (make-hash-table :test #'equal)))
    (multiple-value-bind (start end svect evect) 
        (cl-ppcre:scan "<tri:([^\\s]+)(?:\\s+([A-Za-z]+)=\"([^\\s]+)\")+\\s*>" target :start start-pos)
      (when start
        (do* ((lim (length svect))
              (ind 1 (2+ ind))
              (key-s (aref svect ind))
              (key-e (aref evect ind))
              (val-s (aref svect (1+ ind)))
              (val-e (aref evect (1+ ind))))
             ((> ind length) nil)
          (setf (gethash (subseq target key-s key-e) attr) (subseq target val-s val-e)))
        (setf type (gethash "type" attr))
        (setf prop (gethash "prop" attr))
        (setf n (gethash "nth" attr))
        (setf name (gethash "name" attr))
        (setf func (cond ((String= type "project")
                          #'tripitaka::project)
                         ((String= type "page")
                          (if name
                              #'(lambda (prop) (getf (file2plist name) (string-to-keyword prop)))
                              #'(lambda (prop) (getf plist (string-to-keyword prop)))))))
        (setf result (if n (nth (parse-integer n) (funcall func prop)) (funcall func prop)))
        (scan-templete-cycle (format nil "~A~A~A" (subseq target 0 start) result (subseq target end))
                             (+ start (length result))
                             plist)))))

;;; make new file
;;;
(defun make-new-project (name)
  (let* ((pdir (getf name *project*))
         (conf-file (merge-pathnames ".project" pdir)))
    (with-open-file (out conf-file :direction :output :if-exists :supersede)
      (format out ":site-name \"\"~%")
      (format out ":site-url \"\"~%"))))

(defun make-new-file (name &key
                             (title "")
                             (date (get-date-string "-"))
                             (up (list "" ""))
                             (next (list "" ""))
                             (prev (list "" ""))
                             (uuid "")
                             (list nil)
                             (text "")
                             (overwrite nil))
  (let* ((sym (if nav-flg ":LIST" ":TEXT"))
         (path (merge-pathnames (format nil "~A.dat" name) *dat-dir*)))
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
          (format out "~A" sym)
          (if nav-flg
              (progn (dolist (anchor list) (print anchor out)))
              (format out "~&~A" text))))))


