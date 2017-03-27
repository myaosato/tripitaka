;;;;
;;;;
;;;;

;;; Package
(eval-when (:load-toplevel :compile-toplevel)
  (ql:quickload :cl-fad :silent t)
  (ql:quickload :cl-ppcre :silent t)
  (ql:quickload :local-time :silent t)
  (ql:quickload :cl-markup :silent t)
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
  (cl-fad:pathname-as-directory "~/default-project/"))
(defvar *home-dir* (merge-pathnames "home/" *project-dir*))
(defvar *dat-dir* (merge-pathnames "dat/" *project-dir*))
(defvar *img-dir* (merge-pathnames "img/" *home-dir*))
(defvar *conf-hash* (make-hash-table :test #'equal))
(defvar *project-conf-file*
  (merge-pathnames "project.conf" *project-dir*))
(defvar *icon-conf-file*
  (merge-pathnames "icon.conf" *project-dir*))
(defvar *uuid-file*
  (merge-pathnames "uuids" *project-dir*))
(defvar *uuid-hash* (make-hash-table :test #'equal))
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
         ((or (eql elt :formatted-contents) (not list))
          (append (reverse result) (insert-sep #\Linefeed (cdr list))))
      (push elt result))))

(defun show-hash (hash-table)
  (maphash #'(lambda (key val) (format t "~A: ~A~%" key val)) hash-table))

(defun read-sym-str-file (file hash)
  (with-open-file (in file)
    (do ((sym (read in nil nil) (read in nil nil)))
        ((not sym) (hash-table-count hash))
      (setf (gethash (string-downcase (symbol-name sym)) hash)
            (cl-fad:pathname-as-directory (read-line in nil nil))))))

(defun get-date-string (arg)
  "get date as string YYYYMMDD separated by arg"
  (local-time:format-timestring nil (local-time:now)
                                :format (list '(:year 4) arg '(:month 2) arg '(:day 2))))

;;; System
;;;
(defun message (text)
  (format t "TRIPITAKA: ~A" text))


;;; Initialize
;;;
(defun read-rc ()
  "read .tripitakarc"
  (read-sym-str-file *rc-file* *project-hash*))

(defun read-conf ()
  "read config file"
  (read-sym-str-file *project-conf-file* *conf-hash*))

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
          (icon (read-line in nil nil) (read-line in nil nil)))
         ((not icon)
          (setf *icons*
                (list :div
                      :id "icons"
                      (reverse result))))
      (if (and (getf icon :img) (not (equal (getf icon :img) "")))
          (push (make-icon (getf icon :img)
                           (getf icon :alt)
                           (getf icon :href))
                result)))))

(defun set-project (project)
  "set project config"
  (setf *project-dir* (gethash project *project-hash*))
  (setf *project-conf-file* (merge-pathnames "project.conf" *project-dir*))
  (setf *home-dir* (merge-pathnames "home/" *project-dir*))
  (setf *dat-dir* (merge-pathnames "dat/" *project-dir*))
  (setf *img-dir* (merge-pathnames "img/" *home-dir*))
  (read-conf)
  (setf *copyright*
        (list :div
              :id "copyright"
              ''("&copy;")
              (conf "pub-year")
              " "
              (conf "author")))
  (setf *lang* (conf "lang"))
  (setf *icons* (make-icons)))


(let ((init-flag t))
  (defun start (project)
    "initialize function"
    (if init-flag
        (progn
          (set-project project)
          (setf init-flag nil))))
  (defun init-flag (boolean)
   "set init-flag"
    (setf init-flag boolean)))

;;; Make markup 
;;;
(defun file->art (name)
  "make article data as plist from dat file."
  (let ((art-plist nil))
    (with-open-file (in (merge-pathnames (format nil "~A.art" name) *dat-dir*))
      (do ((elt (read in nil nil) (read in nil nil)))
          ((or (eq elt :text) (not elt)) nil)
        (cond ((or (eq elt :title) (eq elt :date))
               (setf (getf art-plist elt) (read-line in nil nil)))
              ((or (eq elt :up) (eq elt :prev) (eq elt :next))
               (let ((value (cl-ppcre:split " " (read-line in nil nil))))
                 (setf (getf art-plist elt) value)))
              (t nil)))
      (do ((result "" (format nil "~a~%~a" result line))
           (line (read-line in nil nil) (read-line in nil nil)))
          ((not line) (setf (getf art-plist :text) result))))
    art-plist))

(defun date (art)
  (list :div :id "date" (getf art :date)))

(defun title (art)
  (list :h2 :id "title" (getf art :title)))

(defun relation (art)
  (flet ((format-each (type)
           (let ((upn (getf art (intern (string-upcase type) :keyword))))
             (list (format nil "~%~A: "
                               (string-upcase type))
                   (list :a 
                         :href (if upn (second upn) "")
                         (if upn (format nil "~A.htm" (first upn)) ""))))))
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

(defun article-main(art)
  (formatted-element :div
                     :id "article-main"
                     :formatted-contents
                     `'(,(getf art :text))))

(defun main (art &optional (nav-flg nil))
  (formatted-element :main
                     :formatted-contents
                     (article-header art)
                     (if nav-flg
                         (nav-main art)
                         (article-main art))
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

(defun body (art &optional (nav-flg nil))
  (formatted-element :body
                     :formatted-contents
                     (header)
                     (main art nav-flg)
                     (footer)))

(defun head (art)
  (formatted-element :head
                     :formatted-contents
                     *viewport* 
                     *charset*
                     *css-link*
                     *feed-link*
                     (list :title
                           (conf "site-name")
                           " - "
                           (getf art :title))))

(defun page (page &optional (nav-flg nil))
   (formatted-element :html
                      :lang *lang*
                      :formatted-contents
                      (head page)
                      (body page nav-flg)))

;;; make navi page
;;;
(defun file->nav (name)
  "make navi page data as plist from nav file."
  (let ((navi-plist nil))
    (with-open-file (in (merge-pathnames (format nil "~A.nav" name) *dat-dir*))
      (do ((elt (read in nil nil) (read in nil nil)))
          ((or (eq elt :list) (not elt)) nil)
        (cond ((or (eq elt :up) (eq elt :prev) (eq elt :next))
               (let ((value (cl-ppcre:split " " (read-line in nil nil))))
                 (setf (getf navi-plist elt) value)))
              (t
               (setf (getf navi-plist elt) (read-line in nil nil)))))
      (do ((result nil (cons line result))
           (line (read in nil nil) (read in nil nil)))
          ((not line) (setf (getf navi-plist :list) (reverse result)))))
    navi-plist))

(defun link-list (nav)
  (append (list :ul #\Linefeed)
          (loop for link in (getf nav :page-list)
             append (list (list :li
                                (list :a
                                      :href (format nil "~a.htm"
                                                    (first link))
                                      (second link))
                                (third link))
                          #\Linefeed))))

(defun nav-main(nav)
  (formatted-element :div
                     :id "article-main"
                     :formatted-contents
                     (link-list nav)))

;;; make new file
;;;
(defun make-new-project (name)
  (let* ((pdir (gethash name *project-hash*))
         (icon-file (merge-pathnames "icon.conf" pdir))
         (conf-file (merge-pathnames "project.conf" pdir)))
    (with-open-file (out icon-file :direction :output :if-exists :supersede)
      (format out "(:img \"\" :alt \"\" :href \"\")~%"))
    (with-open-file (out conf-file :direction :output :if-exists :supersede)
      (format out ":site-name \"\"~%")
      (format out ":site-url \"\"~%")
      (format out ":pub-year \"\"~%")
      (format out ":author \"\"~%")
      (format out ":lang \"\"~%"))))

(defun make-new-file (name &key
                             (nav-flg nil)
                             (title "") (date (get-date-string "-"))
                             (up "") (next "") (prev "")
                             (uuid "")
                             (type "date") (arg "-") (sep "-") (a-text "file")
                             (list nil) (text "")
                             (overwrite nil))
  (let* ((ext (if nav-flg ".nav" ".art"))
         (oext (if nav-flg ".dat" ".nav"))
         (sym (if nav-flg ":LIST" ":TEXT"))
         (path (merge-pathnames (format nil "~A~A" name ext) *dat-dir*))
         (opath (merge-pathnames (format nil "~A~A" name oext) *dat-dir*)))
    (if (and (not overwrite) (or (probe-file path) (probe-file opath)))
        (message (format nil "name \"~A\" has already been used!" name))
        (with-open-file (out path
                             :direction :output
                             :if-exists :supersede)
          (format out ":TITLE ~A~%" title)
          (format out ":DATE ~A~%" date)
          (format out ":UP ~A~%" up)
          (format out ":PREV ~A~%" prev)
          (format out ":NEXT ~A~%" next)
          (format out ":UUID ~A~%" uuid)
          (when nav-flg
            (format out ":TYPE ~A~%" type)
            (format out ":ARG ~A~%" arg)
            (format out ":SEP ~A~%" sep)
            (format out ":A-TEXT ~A~%" a-text))
          (format out "~A" sym)
          (if nav-flg
              (progn (dolist (anchor list) (print anchor out)))
              (format out "~&~A" text))))))

(defun file->plist (name)
  (cond ((probe-file (merge-pathnames (format nil "~A.art" name) *dat-dir*))
         (file->art name))
        ((probe-file (merge-pathnames (format nil "~A.nav" name) *dat-dir*))
         (file->nav name))
        (t (message "\"~A\" dosen't exist.") nil)))

(defun render-page (page name)
  (if (or (not (getf page :uuid)) (equal (getf page :uuid) ""))
      (setf (getf page :uuid) (gen-uuid)))
  (with-open-file (out (merge-pathnames (format nil "~A.htm" name) *home-dir*)
                       :direction :output
                       :if-exists :supersede)
    (format out "~A" (cl-markup:markup* page)))
  page)

(defun update-html (name)
  (let ((page (file->plist name)))
    (if page
        (render-page page name))))

(defun next-name-helper (new-one last-name sep)
  (cond ((string>= last-name new-one)
         (format nil
                 "~A~A~A"
                 new-one
                 sep
                 (if (string= last-name new-one)
                     1
                     (1+ (parse-integer (subseq last-name
                                                (+ (length new-one)
                                                   (length sep))))))))
        (t new-one)))

(defun next-name (nav)
  (let ((type (getf nav :type))
        (arg (getf nav :arg))
        (sep (getf nav :sep))
        (last-name (first (getf nav :list))))
    (cond ((equal type "date")
           (next-name-helper (get-date-string arg) last-name sep))
          ((equal type "seq")
           (next-name-helper arg last-name sep))
          (t (next-name-date arg last-name sep)))))

(defun make-new-file-of (nav-name)
  (make-new-file (next-name (file->nav nav-name)) :up "nav-name"))

(defmacro make-nav-file (nav-name nav-plist)
  `(make-new-file ,nav-name
                  :overwrite t
                  :nav-flg t
                  ,@(loop for sym in '(:title :date :up :prev :next :uuid :type :arg :sep :a-text :list)
                       append (list sym `(getf ,nav-plist ,sym)))))


(defun update-add-navi (name &key (comment nil) (use-title nil))
  (let* ((pplist (update-html name))
         (nav-name (getf pplist :UP))
         (nav-plist (file->nav nav-name))
         (a-text (getf nav-plist :A-TEXT)))
    (when nav-name
        (setf (getf nav-plist :LIST)
              (cons (list name
                          (cond ((String= a-text "title")
                                 (getf pplist :TITLE))
                                (t name))
                          (cond (use-title (format nil "#~A" (getf pplist :title)))
                                (comment (format nil "#~A" comment))
                                (t "")))
                    (getf nav-plist :LIST)))
        (make-nav-file nav-name nav-plist))))

