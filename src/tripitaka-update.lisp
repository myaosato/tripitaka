(in-package :tripitaka)

;;; v0.0 -> v0.1
(defun trim (plist)
  (do* ((pl plist (cddr pl))
        (key (car pl) (car pl))
        (val (string-trim "\"" (cadr pl)) 
             (string-trim "\"" (cadr pl))))
       ((not key) plist)
    (setf (getf plist key) val)))

(defun transform-prop (filespec)
  (plist-to-file (trim (read-prop-file filespec)) filespec))

(defun move-0-0->0-1 ()
  (let ((rc (transform-prop #p"~/.tripitakarc")))
    nil))

;;; functions for 0.0.2 version
(defun read-sym-str-file (file)
  (with-open-file (in file)
    (read-sym-str-helper in)))

(defun read-sym-str-helper(in &optional (dec nil))
  (do ((plist nil)
       (line (read-line in nil nil) (read-line in nil nil)))
      ((or (not line) (if (functionp dec) (funcall dec line))) plist)
    (cl-ppcre:register-groups-bind (sym val) (":([^\\s]+)\\s+(.*)" line)
      (setf plist (append plist (list (string-to-keyword sym) (read-val val)))))))

(defun read-val (exp-str)
  "still now, just remove \" (and space) at both edge"
  (string-trim "\"" (string-trim " "exp-str)))

;;; Make plist from file 
;;;
;(defun dat-to-plist (name)
;  "make plist as article from file."
;  (let ((plist nil))
;    (with-open-file (in (merge-pathnames (format nil "~A.dat" name) *dat-dir*))
;      (setf plist (read-sym-str-helper in #'find-text-keyword))
;      (do* ((line (read-line in nil nil) (read-line in nil nil))
;            (result (if line line "") (if line (format nil "~a~%~a" result line) result)))
;          ((not line)
;           (setf (getf plist :text) result)))
;      plist)))
