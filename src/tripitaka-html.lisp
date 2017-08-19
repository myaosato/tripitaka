(in-package :tripitaka)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *no-end-tags* (make-hash-table))
  (defvar *no-end-tag-list* 
    '(:br :img :hr :meta :input :embed :area :base :col :keygen :link :param :source))
  (mapcar (lambda (key) (setf (gethash key *no-end-tags*) t))
          *no-end-tag-list*))
(defvar *open-tag-format* "<~A~{ ~A~:[~;=~:*~S~]~}>")
(defvar *no-end-tag-format* "<~A~{ ~A~:[~;=~:*~S~]~} />")

(defun make-html-from-stream (stream)
  (make-html (read stream)))

(defun make-html-from-file (path)
  (with-open-file (in path)
    (make-html-from-stream in)))

(defun eval-in-make-html (sexp)
  (eval-as-cl-with-your-eval sexp :eval-fn #'make-html))

(defun eval-as-cl-with-your-eval (sexp &key (eval-fn #'eval))
  (apply (get-function (car sexp)) (mapcar eval-fn (cdr sexp))))

(defun get-function (sexp)
  (cond ((symbolp sexp)
         (symbol-function sexp))
        ((eq 'lambda (car sexp))
         (eval sexp))))

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
    ((no-end-tag-p (car sexp-html))
     (format nil "~A" (no-end-tag (car sexp-html) (cadr sexp-html))))
    ((keywordp (car sexp-html))
     (format nil "~A~{~A~}~A" 
             (open-tag (car sexp-html) (cadr sexp-html)) 
             (mapcar #'make-html (cddr sexp-html))
             (close-tag (car sexp-html))))
    (t
     (eval-in-make-html sexp-html)))) 
