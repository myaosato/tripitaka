(in-package :tripitaka)

;; var

(eval-when (:load-toplevel :compile-toplevel)
  (defvar *tri-functions* (make-hash-table))
  (defvar *no-end-tags* (make-hash-table))
  (defvar *no-end-tag-list* 
    '(:br :img :hr :meta :input :embed :area :base :col :keygen :link :param :source))
  (defvar *open-tag-format* "<~A~{ ~A~:[~;=~:*~S~]~}>")
  (defvar *no-end-tag-format* "<~A~{ ~A~:[~;=~:*~S~]~}/>"))

;; func

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

;; initialize

(mapcar (lambda (key) (setf (gethash key *no-end-tags*) t)) *no-end-tag-list*)
