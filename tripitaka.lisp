;;;;
;;;;
;;;;

;;; Package
(eval-when (:load-toplevel :compile-toplevel)
  (ql:quickload :cl-fad :silent t)
  (ql:quickload :cl-ppcre :silent t)
  (ql:quickload :local-time :silent t)
  (ql:quickload :cl-markup :silent t)
  (ql:quickload :uuid :silent t))
(defpackage :tripitaka
  (:use :common-lisp)
  (:export ))
(in-package :tripitaka)

;;; Define special valiable
;;;
(defvar *main-conf-file* (merge-pathnames #p".myao-ssg/main.conf" (user-homedir-pathname)))
(defvar *project-hash* (make-hash-table :test #'equal))
(defvar *css-link*
  (list :link
        :rel "stylesheet"
        :type "text/css"
        :href "style.css"))
(defvar *feed-link*
  (list :link
        :rel "alternate"
        :type "application/atom+xml"
        :title "Atom 1.0"
        :href "feed.xml"))
(defvar *viewport*
  (list :meta
        :name "viewport"
        :content "width=device-width, initial-scale=1, user-scalable=yes"))
(defvar *charset*
  (list :meta
        :charset "UTF-8"))
(defvar *project-dir*
  (cl-fad:pathname-as-directory "~/.myao-ssg/default-project/"))
(defvar *home-dir* (merge-pathnames "home/" *project-dir*))
(defvar *dat-dir* (merge-pathnames "dat/" *project-dir*))
(defvar *img-dir* (merge-pathnames "img/" *home-dir*))
(defvar *conf-hash* (make-hash-table :test #'equal))
(defvar *project-conf-file*
  (merge-pathnames "main.conf" *project-dir*))
(defvar *icon-conf-file*
  (merge-pathnames "icon.conf" *project-dir*))
(defvar *icons* "")
(defvar *site-url* "")
(defvar *copyright* nil)
(defvar *site-name* "")
(defvar *lang* nil)
;;; Utils
;;;
(defun insert-sep (sep input-list) 
  (do* ((result)
        (list input-list (cdr list))
        (elt (car list) (car list)))
       ((not list) (progn (push sep result)
                          (reverse result)))
    (push sep result)
    (push elt result)))

(defun formatted-element (elt-name &rest rest)
  (let ((result (list elt-name)))
    (do* ((list rest (cdr list))
          (elt (car list) (car list)))
         ((or (eql elt :formatted-contents) (not list)) (append (reverse result) (insert-sep #\Linefeed (cdr list))))
      (push elt result))))

;;; Initialize
;;;
(defun read-rc ()
  (with-open-file (in *main-conf-file*)
    (do ((line (read-line in nil nil) (read-line in nil nil)))
        ((not line) nil)
      (cl-ppcre:register-groups-bind (key value)
          ("\\s*[^=\\s]+\\s*=\\s*[^\\s]+" line)
        (setf (gethash key *project-hash*) value)))))

(defun set-project (project)
  (declare (ignore project))
  (let ()
    (setf *project-dir*
          (cl-fad:pathname-as-directory "~/WEBSITE/myao-info/"))))

(defun read-conf ()
  "read config file"
    (with-open-file (in *project-conf-file*)
      (do ((line (read-line in nil nil)))
          ((not line) nil)
        (cl-ppcre:register-groups-bind (key val)
            ("\\s*[^=\\s]+\\s*=\\s*[^\\s]+" line)
          (setf (gethash (string-downcase key) *project-conf-file*) val)))))

(defun conf (name)
  "get value from config"
  (gethash name *conf-hash*))

(defun make-icon (img alt href)
  "make element for an icon"
  (list :a
        :href href 
        (list :img
              :src (format nil "~A~A" "img/" img)
              :alt alt)))

(defun make-icons ()
  "make element for icon"
  (with-open-file (in *icon-conf-file*)
    (do* ((result)
          (icon (read in nil nil) (read in nil nil)))
         ((not icon)
          (setf *icons*
                (list :div
                      :id "icons"
                      (reverse result))))
      (push (make-icon (getf icon :img)
                       (getf icon :alt)
                       (getf icon :href))
            result))))

(let ((init-flag t))
  (defun init (project)
    "initialize function"
    (if init-flag
        (progn
          (set-project project)
          (setf *home-dir* (merge-pathnames "home/" *project-dir*))
          (setf *dat-dir* (merge-pathnames "dat/" *project-dir*))
          (setf *img-dir* (merge-pathnames "img/" *home-dir*))
          (setf *project-conf-file*
            (merge-pathnames "main.conf" *project-dir*))
          (setf *site-url* (conf "site-url"))
          (setf *copyright*
            (list :div
                  :id "copyright"
                  ''("&copy;")
                  (conf "pub-year")
                  " "
                  (conf "author")))
          (setf *lang* (conf "lang"))
          (setf *site-name* (conf "site-name"))
          (setf *icons* (make-icons))
          (setf init-flag nil))))
  (defun init-flag (boolean)
   "set init-flag"
    (setf init-flag boolean)))

;;; Make html-strings from dat
;;;
(defun dat->article-plist (name)
  "make article data as plist from dat file."
  (let ((art-plist nil))
    (with-open-file (in (merge-pathnames name *dat-dir*))
      (do ((elt (read in nil nil) (read in nil nil)))
          ((or (eq elt :text) (not elt)) nil)
        (setf art-plist (append art-plist elt)))
      (do ((result "" (format nil "~a~%~a" result line))
           (line (read-line in nil nil) (read-line in nil nil)))
          ((not line) (setf (getf art-plist :text) result))))
    art-plist))

(defun date (art)
  (list :div
        :id "date"
        (getf art :date)))

(defun title (art)
  (list :h2
        :id "title"
        (getf art :title)))

(defun relation (art)
  (flet ((format-each (type)
           (let ((upn (getf art (intern (string-upcase type) :keyword))))
             (list (format nil "~%~A: "
                               (string-upcase type))
                   (list :a 
                         :href (second upn)
                         (format nil "~A.htm" (first upn)))))))
    (append (list :div
                  :id "relation")
            (append (format-each "UP")
                    (format-each "PREV")
                    (format-each "NEXT"))
            (list #\Linefeed))))

(defun article-header (art)
  (formatted-element :header
                     :id "article-header"
                     :formatted-contents
                     (date art)
                     (title art)))

(defun article-footer (art)
  (formatted-element :footer
                     :id "article-footer"
                     :formatted-contents
                      (relation art)))

(defun article-main (art)
  (formatted-element :div
                     :id "article-main"
                     :formatted-contents
                     `'(,(getf art :body))))

(defun article (art)
  (formatted-element :main
                     :formatted-contents
                     (article-header art)
                     (article-main art)
                     (article-footer art)))

(defun header ()
  (formatted-element :header
                     :id "main-header"
                     :formatted-contents
                      `(:h1
                        (:a
                         :href "index.htm"
                         ,*site-name*))))

(defun footer ()
  (formatted-element :footer
                     :id "main-footer"
                     :formatted-contents
                     *icons*
                     *copyright*))

(defun body (art)
  (formatted-element :body
                     :formatted-contents
                     (header)
                     (article art)
                     (footer)))

(defun head (art)
  (formatted-element :head
                     :formatted-contents
                     *viewport* 
                     *charset*
                     *css-link*
                     *feed-link*
                     (list :title
                           *site-name*
                           " - "
                           (getf art :title))))

(defun page (page)
   (formatted-element :html
                      :lang *lang*
                      :formatted-contents
                      (head page)
                      (body page)))

;;; Definition of functions for update XML (RSS 2.0) feed
;;;
(defun rfc822-date-format ()
  (local-time:format-timestring
   nil 
   (local-time:now)
   :format '(:short-weekday ", "
             :day " "
             :short-month " "
             :year " "
             :hour ":"
             :min ":"
             :sec " "
             :gmt-offset-hhmm)))

;;; Definition of funcitons for update XML (Atom 1.0) feed
;;;
(defun gen-uuid ()
  (let ()
    (uuid:format-as-urn nil (uuid:make-v1-uuid))))

(defun atom-date-now ()
  (local-time:format-timestring nil (local-time:now)))
