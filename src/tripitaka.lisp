;;; PACKAGE
(in-package :cl-user)

(defpackage :tripitaka
  (:use :common-lisp)
  (:export ))

(in-package :tripitaka)

;;; VARIABLE
(eval-when (:load-toplevel :compile-toplevel)
  (defvar *rc-file* (merge-pathnames #p".tripitakarc"
                                     (user-homedir-pathname)))  
  (defvar *charset-utf8*
  #+(or sbcl ccl cmu allegro ecl lispworks) :utf-8
  #+clisp charset:utf-8)
  (defvar *file-cache* (make-hash-table :test 'equal))
  (defvar *projects-plist* nil)
  (defvar *project-dir* nil)
  (defvar *theme-dir* nil)
  (defvar *project-file* nil)
  (defvar *project* nil)
  (defvar *dat-dir*)
  (defvar *html-dir*)
  (defvar *sync-file*)
  (defvar *tri-functions* (make-hash-table))
  (defvar *no-end-tags* (make-hash-table))
  (defvar *no-end-tag-list* 
    '(:br :img :hr :meta :input :embed :area :base :col :keygen :link :param :source))
  (defvar *open-tag-format* "<~A~{ ~A~:[~;=~:*~S~]~}>")
  (defvar *no-end-tag-format* "<~A~{ ~A~:[~;=~:*~S~]~}/>")
  (defvar *feed-dat* nil)
  (defvar *feed-atom* nil)
  (defvar *gen-id* (lambda () nil)))

;;; UTIL
(defun get-date-string (arg)
  "get date as string YYYYMMDD separated by arg"
  (local-time:format-timestring nil (local-time:now)
                                :format (list '(:year 4) arg '(:month 2) arg '(:day 2))))

(defun make-dir (dir-path)
  (second (multiple-value-list (ensure-directories-exist
                                (cl-fad:pathname-as-directory dir-path)))))

;;; MARKDOWN
(defun md-to-html-string (target)
  (nth 1 (multiple-value-list (cl-markdown:markdown target :stream nil))))

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

;;; CONVERTER
(defun no-end-tag-p (key)
  (gethash key *no-end-tags*))

(defun open-or-no-end-tag (keyword attr-list &optional (format-str *open-tag-format*))
  (let ((name (string-downcase (symbol-name keyword)))
        (attrs (mapcar (lambda (elt) 
                           (if (symbolp elt) 
                               (string-downcase (symbol-name elt)) 
                               elt)) 
                       attr-list)))
    (format nil format-str name attrs)))

(defun open-tag (keyword attr-list)
  (open-or-no-end-tag keyword attr-list *open-tag-format*))

(defun no-end-tag (keyword attr-list)
  (open-or-no-end-tag keyword attr-list *no-end-tag-format*))

(defun close-tag (keyword)
  (format nil "</~A>" (string-downcase (symbol-name keyword))))

(defun make-html (sexp-html)
  (cond 
    ((stringp sexp-html)
     (format nil "~A" sexp-html))
    ((gethash sexp-html *tri-functions*)
     (gethash sexp-html *tri-functions*))
    ((no-end-tag-p (car sexp-html))
     (format nil "~A" (no-end-tag (car sexp-html) (cadr sexp-html))))
    ((keywordp (car sexp-html))
     (format nil "~A~{~A~}~A" 
             (open-tag (car sexp-html) (cadr sexp-html)) 
             (mapcar #'make-html (cddr sexp-html))
             (close-tag (car sexp-html))))
    ((gethash (car sexp-html) *tri-functions*)
     (apply (gethash (car sexp-html) *tri-functions*) (mapcar #'make-html (cdr sexp-html))))
    ((eq (car sexp-html) 'if) 
     (if (make-html (cadr sexp-html)) (make-html (caddr sexp-html)) (make-html (cadddr sexp-html))))
    ((eq (car sexp-html) '=)
     (String= (make-html (car sexp-html)) (make-html (cadr sexp-html)))) 
    (t
     "")))

(defun make-html-from-stream (stream)
  (make-html (read stream)))

(defun make-html-from-file (path)
  (with-open-file (in path)
    (make-html-from-stream in)))

(defmacro deftrifun (name args &body body) 
  `(setf (gethash ',name *tri-functions*) (lambda ,args ,@body)))

;;; INITIALIZE
;; NO-END-TAGS
(mapcar (lambda (key) (setf (gethash key *no-end-tags*) t)) *no-end-tag-list*)

;; FUNCTIONS FOR CONVERTER
(deftrifun get-prop (name prop) 
  (get-prop name prop))

(deftrifun get-prop-as-list (name prop) 
  (get-prop-as-list name prop))

(deftrifun dolist (lst func)
  (format nil "~{~A~}" (mapcar func lst)))

(deftrifun anchor (href-label)
  (make-html (list :a (list :href (first href-label)) (second href-label))))

(deftrifun sexp-list (&rest elts)
  (apply #'list elts))

(deftrifun ul (list)
  (make-html 
   '(list :ul ()  
     (loop for elt on list 
           collect '(:li () elt)))))









