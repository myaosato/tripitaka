;;; PACKAGE
(in-package :cl-user)

(defpackage :tripitaka
  (:use :common-lisp)
  (:export :update-all
           :update))

(in-package :tripitaka)

;;; VARIABLE
(eval-when (:load-toplevel :compile-toplevel)
  (defvar *charset-utf8*
    #+(or sbcl ccl cmu allegro ecl lispworks) :utf-8
    #+clisp charset:utf-8)
  (defvar *project-dir*)
  (defvar *project-file*)
  (defvar *file-cache* (make-hash-table :test 'equal))
  (defvar *theme-dir* nil)
  (defvar *current-file-name* "")
  (defvar *dat-dir*)
  (defvar *html-dir*)
  (defvar *template-dir*)
  (defvar *sync-file*)
  (defvar *ignore* (make-hash-table))
  (defvar *templetes* (make-hash-table))
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

(defun current-dir ()
  (pathname (truename "./")))

(defun exist-file-in-dir (name dir) 
  (some (lambda (elt) 
          (equal (merge-pathnames name dir) elt))
        (cl-fad:list-directory dir)))

;;; MARKDOWN
(defun md-to-html-string (target)
  (nth 1 (multiple-value-list (cl-markdown:markdown target :stream nil))))

;;; DATA
(defun file->data (path)
  (with-open-file (in path)
    (rosa:peruse-as-plist in #'string-upcase)))

(defun name->data (name)
  (let ((data (gethash name *file-cache*)))
    (if data
        data
        (setf (gethash name *file-cache*) (file->data (get-data-path name))))))

(defun data->file (data path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (princ (rosa:indite data) out)))

(defun data->file-by-name (data name)
  (with-open-file (out (get-data-path name) :direction :output :if-exists :supersede)
    (princ (rosa:indite data) out)))

(defun string-to-keyword (str)
  (eval (read-from-string (format nil ":~A" (string-upcase str)))))

(defun get-prop-from-data (data prop &optional (type 'string))
  (cond ((eql type 'list) (coerce (getf data (string-to-keyword prop)) 'list))
        ((eql type 'vector) (getf data (string-to-keyword prop)))
        (t (aref (getf data (string-to-keyword prop)) 0))))

(defun get-prop-as-list (name prop)
  (get-prop-from-data (name->data name) prop 'list))

(defun get-prop (name prop)
  (get-prop-from-data (name->data name) prop 'string))

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
  (mapcar #'(lambda (pathname) (list pathname
                                     (get-file-name pathname)
                                     (file-write-date pathname)
                                     (get-type-or-dir pathname)))
          (cl-fad:list-directory (cl-fad:pathname-as-directory dir))))

(defun get-files (dir extension)
  (let ((all-list (get-children-list dir)))
    (append 
     (remove-if-not (lambda (elt) (equal (nth 3 elt) extension)) all-list))))

(defun get-data-files ()
  (get-files *dat-dir* "rosa"))

(defun get-html-files ()
  (get-files *html-dir* "html"))

;;; MANAGE 
(defun get-rosa-file-as-hashtable (file)
  (with-open-file (in file)
    (rosa:peruse in)))

(defun get-sync-hash ()
  (get-rosa-file-as-hashtable *sync-file*))

(defun get-convert-time (name)
  (aref
   (gethash (string-to-symbol name) (get-rosa-file-as-hashtable *sync-file*))
   0))

(defun sava-hashtable-as-rosa-file (hashtable file)
  (with-open-file (out file :direction :output :if-exists :supersede)
    (rosa:indite hashtable)))

(defun string-to-symbol (str)
  (eval (read-from-string (format nil "'~A" str))))
  
(defun %registor-convert-time (name timestamp)
  (let ((sync-hash (get-rosa-file-as-hashtable *sync-file*)))
    (setf (gethash (string-to-symbol name) sync-hash)
          timestamp)
    (sava-hashtable-as-rosa-file sync-hash *sync-file*))
  timestamp)

(defun registor-convert-time (name)
  (%registor-convert-time name 
                          (file-write-date (get-data-path name))))

(defun is-converted (name)
  (String= (get-convert-time name) (file-write-date (get-data-path name))))

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

(defun htmlisp (sexp-html)
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
             (mapcar #'htmlisp (cddr sexp-html))
             (close-tag (car sexp-html))))
    ((gethash (car sexp-html) *tri-functions*)
     (apply (gethash (car sexp-html) *tri-functions*) (mapcar #'htmlisp (cdr sexp-html))))
    ((eq (car sexp-html) 'if) 
     (if (htmlisp (cadr sexp-html)) (htmlisp (caddr sexp-html)) (htmlisp (cadddr sexp-html))))
    ((eq (car sexp-html) '=)
     (String= (htmlisp (car sexp-html)) (htmlisp (cadr sexp-html)))) 
    (t
     "")))

(defun convert-to-html-from-stream (stream)
  (htmlisp (read stream)))

(defun convert-into-html-from-file (path)
  (with-open-file (in path)
    (convert-to-html-from-stream in)))

(defmacro depftrifun (name args &body body) 
  `(setf (gethash ',name *tri-functions*) (lambda ,args ,@body)))

;;; INITIALIZE
;; NO-END-TAGS
(mapcar (lambda (key) (setf (gethash key *no-end-tags*) t)) *no-end-tag-list*)

;; FUNCTIONS FOR CONVERTER
(deftrifun get-prop (prop name type) 
  (if (String= name "this")
      (setf name *current-file-name*))
  (if (String= type "md")
      (cl-markdown:markdown (get-prop name prop))
      (get-prop name prop)))

(deftrifun get-prop-as-list (name prop) 
  (get-prop-as-list name prop))

(deftrifun each (func lst)
  (format nil "~{~A~}" (mapcar func lst)))

(deftrifun anchor (href label)
  (htmlisp (list :a (list :href href) label)))

(deftrifun sexp-list (&rest elts)
  (apply #'list elts))

(deftrifun ul (list)
  (htmlisp 
   (list :ul ()  
         (loop for elt on list 
               collect '(:li () elt)))))

(deftrifun string-add (str1 str2)
  (concatenate 'string str1 str2))

;;; PATH
(defun get-data-path (name)
  (merge-pathnames (format nil "~A.rosa" name) *dat-dir*))

(defun get-html-path (name)
  (merge-pathnames (format nil "~A.html" name) *html-dir*))

(defun get-template-path (name) 
  (merge-pathnames name *template-dir*))

;;; ENVIROMENT
(defun setenv (project-dir)
  (setf *project-file* (merge-pathnames ".tripitaka" project-dir))
  (setf *dat-dir* (merge-pathnames "dat/" project-dir))
  (setf *html-dir* (merge-pathnames "home/" project-dir))
  (setf *template-dir* (merge-pathnames "template/" project-dir))
  (setf *sync-file* (merge-pathnames "sync.rosa" project-dir)))

;;; SETTING
(defun %find-project-dir (path-string)
  (if (exist-file-in-dir ".tripitaka" (pathname (truename path-string)))
      path
      (if (equal path #P"/")
          nil
          (%findproject-dir (concatenate 'string "../" path-string)))))

(defun find-project-dir ()
  (%findproject-dir "./"))

(defun initialize ()
  (let ((project-dir (findproject-dir)))
    (if project-dir
        (setenv project-dir))))

;;; WRITE HTML

(defun read-template (template-name)
  (let ((tamplate-path (if template-name
                           (get-template-path template-name)
                           (get-template-path "template"))))
    (with-open-file (in tamplate-path :direction :input)
        (read in))))

(defun post-proc (name)
  nil)

(defun dat-to-html (name &optional template-name)
 (let ((*current-file-name* name))
   (with-open-file (out (get-html-path name) :direction :output :if-exists :supersede)
     (format out "<!doctype html>~%")
     (format out "~A" (convert-to-html-from-stream (read-template template-name))))
   (registor-convert-time name)
   (post-proc name)))

(defun update-all ()
  (mapcar (lambda (elt) (dat-to-html (pathname-name (car elt))))
          (get-data-files)))

(defun update ()
  (mapcar (lambda (elt) (if (is-converted (pathname-name (car elt)))  
                            nil 
                            (dat-to-html (pathname-name (car elt)))))
          (get-data-files)))

;;; ATOM
;;;; TODO 

;;; AGGREGATION
;;;; TODO

