;;;; simple ftp client
;;;; Recently this is test(developping) version
;;;;
(eval-when (:load-toplevel :compile-toplevel)
  (ql:quickload :cl-ftp :silent t)
  (ql:quickload :usocket :silent t)
  (ql:quickload :cl-ppcre :silent t)
  (ql:quickload :cl-fad :silent t))
(defpackage :tripitaka-ftp
  (:use :common-lisp)
  (:export))

(in-package :tripitaka-ftp)

(defun format-hash-key-value (stream hash-map)
  (maphash #'(lambda (key value) (format stream "~a: ~a~%" key value)) hash-map))

(defun get-type-mod-name (line) 
  (cl-ppcre:register-groups-bind
   (mod type name) ("modify=(\\d{14}).*type=([^;]*);.*; ([^\\n\\r]*)" line)
   (list type name mod)))

(defun list->hash (list &key (test #'eql))
  (let ((result (make-hash-table :test test)))
    (do* ((remain list (rest remain))
          (key (first (first remain)) (first (first remain)))
          (value (second (first remain)) (second (first remain))))
        ((not key) result)
      (setf (gethash key result) value))))

(defun mlsd->list (s)
  (let (dlist flist)
    (loop (handler-case
              (let ((file-list (get-type-mod-name (read-line (usocket::socket-stream s)))))
                (cond
                  ((equal (first file-list) "dir")
                   (push (rest file-list) dlist))
                  ((or (equal (first file-list) "pdir")
                       (equal (first file-list) "cdir")))
                  (t
                   (push (rest file-list) flist))))
            (end-of-file () (return (list dlist flist)))))))

(defun get-remote-info-hash (conn &optional (pathname ""))
  (let (result)
    (ftp:with-transfer-socket (s conn (format nil "MLSD ~A" pathname)
                             :type :ascii)
      (setf result (mlsd->list s)))
    (list (list->hash (first result) :test #'equal)
          (list->hash (second result) :test #'equal))))

(defun get-update-time-local (pathname)
  (multiple-value-bind
        (second minute hour date month year)
      (decode-universal-time (file-write-date pathname) 0)
    (format nil "~A~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D"
            year month date hour minute second)))

(defun local-files->list (pathname)
  (let (dlist
        flist) 
    (loop for pn in (cl-fad:list-directory pathname)
       do (let* ((file-name (enough-namestring (cl-fad:pathname-as-file pn)
                                               (cl-fad:pathname-as-directory pathname))))
            (if (cl-fad:directory-pathname-p pn)
                (push (list file-name "DIR") dlist)
                (push (list file-name (get-update-time-local pn)) flist))))
         (list dlist flist)))

(defun get-local-info-hash (pathname)
  (let ((list (local-files->list pathname)))
    (list (list->hash (first list) :test #'equal)
          (list->hash (second list) :test #'equal))))

(defun time< (time1 time2)
  (< (parse-integer time1) (parse-integer time2)))

(defun compare-files (conn local-files remote-files local-dir)
  (loop for name being the hash-keys in local-files
       using (hash-value local-time)
     do (let ((remote-time (gethash name remote-files))
              (local-file (merge-pathnames name local-dir)))
          (cond
            ((not remote-time)
             (ftp:store-file conn local-file name))
            ((time< remote-time local-time)
             (ftp:store-file conn local-file name)
             (remhash name remote-files))
            (t
             (remhash name remote-files)))))
  (loop for name being the hash-keys in remote-files
       do (ftp:send-dele-command conn name)))

(defun compare-dirs (conn local-dirs remote-dirs local-dir)
  (loop for name being the hash-keys in local-dirs
     do (let ((remote-time (gethash name remote-dirs)))
          (cond
            ((not remote-time)
             (ftp:send-mkd-command conn name)
             (ftp:send-cwd-command conn name)
             (mu-help conn (merge-pathnames name local-dir) :ignore-remote t))
            (t
             (remhash name remote-dirs)
             (ftp:send-cwd-command conn name)
             (mu-help conn (merge-pathnames name local-dir))))))
  (loop for name being the hash-keys in remote-dirs
     do (progn 
         (ftp:send-cwd-command conn name)
         (mu-help conn (merge-pathnames name local-dir) :ignore-local t)
         (ftp:call-with-transfer-socket conn (format nil "RMD ~A" name) #'(lambda ())))))

(defun mu-help (conn local-dir &key (root-flag nil) (ignore-remote nil) (ignore-local nil))
  (let* ((local-dir (cl-fad:pathname-as-directory local-dir))
         (dummy-hash (make-hash-table :test #'equal))
         (dh-list (list dummy-hash dummy-hash))
         (local-info (if ignore-local dh-list (get-local-info-hash local-dir)))
         (remote-info (if ignore-remote dh-list (get-remote-info-hash conn)))
         (local-dirs (first local-info))
         (local-files (second local-info))
         (remote-dirs (first remote-info))
         (remote-files (second remote-info)))
    (compare-files conn local-files remote-files local-dir)
    (compare-dirs conn local-dirs remote-dirs local-dir)
    (if root-flag
        (print "finish")
        (ftp:send-cdup-command conn))))

(defun mirroring-upload (hostname password username home-dir)
  (ftp.client:with-ftp-connection
      (conn :hostname hostname
            :password password
            :username username
            :passive-ftp-p t)
    (mu-help conn home-dir :root-flag t)))






