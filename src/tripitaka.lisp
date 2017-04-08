;;;;
;;;;
;;;;

;;; Package
(in-package :tripitaka)

;;; Define special valiable
;;;
(defvar *rc-file* (merge-pathnames #p".tripitakarc"
                                          (user-homedir-pathname)))
(defvar *projects-plist* nil)
(defvar *project-dir*
  (cl-fad:pathname-as-directory "~/default-project/"))
(defvar *home-dir* "")
(defvar *dat-dir* "")
(defvar *theme-dir* "")
(defvar *project-file* "")
(defvar *project* nil)
(defvar *feed-dat* "")
(defvar *feed-atom* "")
(defvar *gen-id* (lambda () nil))

;;; Utils
;;;
(defun read-sym-str-file (file)
  (with-open-file (in file)
    (do ((plist nil)
         (sym (read in nil nil) (read in nil nil)))
        ((not sym) plist)
      (setf plist (append plist (list sym (read in nil nil)))))))

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

(defun make-dir (dir-path)
  (ensure-directories-exist (cl-fad:pathname-as-directory dir-path)))

(defun md-to-html-string (target)
  (nth 1 (multiple-value-list (cl-markdown:markdown target :stream nil))))

;;; System
;;;
(defun message (text &optional (result nil))
  (format t "TRIPITAKA: ~A~%" text)
  result)

;;; Initialize
;;;
(defun read-rc ()
  "read .tripitakarc"
  (setf *projects-plist* (read-sym-str-file *rc-file*))
  (symbol-name (first *projects-plist*)))

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
  (setf *theme-dir* (merge-pathnames "theme/" *project-dir*))
  (setf *feed-dat* (merge-pathnames "feed" *project-dir*))
  (setf *feed-atom* (merge-pathnames "feed.xml" *home-dir*))
  (load-project))

(defun project (name)
  "get value from config"
  (getf *project* (string-to-keyword name)))

(defun set-id-func ()
  (setf *gen-id* (lambda () (gen-id-uuid-v4))))

(defun ready ()
  (set-project (read-rc))
  (set-id-func)
  (message "preparation is finished"))

;;; Utils for tripitaka
;;;
(defun find-dat (name)
  "if name.dat dosen't exist return nil"
  (probe-file (merge-pathnames (format nil "~A.dat" name) *dat-dir*)))

(defun find-html (name)
  "if name.htm dosen't exist return nil"
  (probe-file (merge-pathnames (format nil "~A.htm" name) *home-dir*)))

(defun find-text-keyword (line)
  (cl-ppcre:scan "^:TEXT" (string-trim " " (string-upcase line))))

(defun file-list (&optional (regex "[^~]$") (dir *dat-dir*))
  (loop for path in (cl-fad::list-directory dir)
     when (cl-ppcre:scan regex (namestring path))
     collect (pathname-name path)))

(defun nil-to-zero (obj)
  (if obj obj 0))

(defun diary> (diary1 diary2)
  (let ((lst1 (mapcar #'parse-integer (cl-ppcre:split "-" diary1)))
        (lst2 (mapcar #'parse-integer (cl-ppcre:split "-" diary2))))
    (cond ((> (first lst1) (first lst2)) t)
          ((< (first lst1) (first lst2)) nil)
          ((> (nil-to-zero (second lst1)) (nil-to-zero (second lst2))) t)
          (t nil))))

(defun get-file-path (name)
  (merge-pathnames (format nil "~A.dat" name) *dat-dir*))

(defun get-url (name)
  (format nil "~A/~A.htm" (string-right-trim "/" (project "site-url")) name))

;;; Make plist from file 
;;;

(defun dat-to-plist (name)
  "make plist as article from file."
  (let ((plist nil) sym val)
    (with-open-file (in (merge-pathnames (format nil "~A.dat" name) *dat-dir*))
      (do ((line (read-line in nil nil) (read-line in nil nil)))
          ((or (find-text-keyword line) (not line)) plist)
        (with-open-stream (in-line (make-string-input-stream line))
          (setf sym (read in-line nil nil))
          (setf val (read in-line nil nil))
          (setf (getf plist sym) (if val val ""))))
      (do ((result "" (format nil "~a~%~a" result line))
           (line (read-line in nil nil) (read-line in nil nil)))
          ((not line)
           (setf (getf plist :text) result)))
    plist)))

;;; Make new files
;;;
(defun make-project (name)
  (let* ((pdir (getf *project* name))
         (conf-file (merge-pathnames ".project" pdir)))
    (with-open-file (out conf-file :direction :output :if-exists :supersede)
      (format out ":site-name \"\"~%")
      (format out ":site-url \"\"~%")
      (format out ":author \"\"~%")
      (format out ":pubyear \"\"~%")
      (format out ":id \"\"~%"))))

(defun plist-to-dat (name plist overwrite)
  (if (and (not overwrite) (find-dat name))
      (message (format nil "~a.dat exists." name))
      (with-open-file (out (get-file-path name) :direction :output :if-exists :supersede)
        (do ((n 0 (+ n 2)))
            ((>= n (length plist)) )
          (if (not (eq (nth n plist) :text))
              (format out "~S ~S~%" (nth n plist) (nth (1+ n) plist))))
        (format out ":TEXT~%")
        (format out "~A" (getf plist :text))
        t)))

(defun make-dat (name &key
                        (title "")
                        (date (get-date-string "-"))
                        (up "")
                        (next "")
                        (prev "")
                        (id "")
                        (text "")
                        (overwrite nil))
  (plist-to-dat name
                (list :title title
                      :date date
                      :up up
                      :next next
                      :prev prev
                      :id id
                      :text text)
                overwrite))


;;; make HTML String
;;;
;;;
(defun scan-template (target start-pos plist &optional (count 0))
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
                               #'project)
                              ((String= type "page")
                               (cond ((not name)
                                      #'(lambda (prop) (getf plist (string-to-keyword prop))))
                                     ((find-dat name)
                                      #'(lambda (prop) (getf (dat-to-plist name) (string-to-keyword prop))))
                                     (t #'any-to-blank)))
                              (t #'any-to-blank)))
             (setf result (if (String= prop "text")
                              (md-to-html-string (funcall func prop))
                              (if n (nth (parse-integer n) (funcall func prop)) (funcall func prop))))
             (scan-template (format nil "~A~A~A" (subseq target 0 start) (if result result "") (subseq target end))
                            (+ start (length result))
                            plist
                            (1+ count)))
            ((= count 0)
              target)
             (t
              (scan-template target
                             0
                             plist))))))

(defun make-from-template (name template-name)
  (let ((target (file2string (merge-pathnames  template-name  *theme-dir*))))
    (scan-template target
                   0
                   (dat-to-plist name))))

(defun make-html (name &optional (template-name "template"))
  "make html file"
  (if (find-dat name)
      (with-open-file (out (merge-pathnames (format nil "~A.htm" name) *home-dir*)
                           :direction :output
                           :if-exists :supersede)
        (format out "~A" (make-from-template name template-name)))
      (message (format nil "name \"~A\" can't be found!" name))))

;;; Static Site Manager
;;;
;;;

(defun diary-list ()
  (sort (file-list "\\d{8}(-\\d+)?.dat$") #'diary>))

(defun diary-list-from-page ()
  (sort (if (find-dat "diary")
            (mapcar #'(lambda (line)
                        (cl-ppcre:register-groups-bind (result) ("\\[(.*).htm\\]" line)
                          result))
                    (cl-ppcre:split (format nil "~%") (getf (dat-to-plist "diary") :text))))
        #'diary>))

(defun make-name (&optional (pre (get-date-string "")) num)
  (let ((name (format nil "~A~A" pre (if num (format nil "-~A" num) ""))))
    (if (find-html name)
        (make-name pre 1)
        name)))

(defun make-new-diary ()
  (let ((name (make-name))
        (prev (first (diary-list))))
    (if (make-dat name :up "diary" :prev prev)
        (message (format nil "make ~A~A.dat" *dat-dir* name) t))))

(defun update-page (name &optional (template-name "template"))
  (let* ((page (dat-to-plist name))
         (id (getf page :id)))
    (if (or (not id) (equal "" id))
        (setf (getf page :id) (funcall *gen-id*)))
    (plist-to-dat name page t)
    (make-html name template-name)))

(defun load-diary ()
  (unless (find-dat "diary")
    (make-dat "diary" :title "DIARY"))
  (dat-to-plist "diary"))

(defun update-diary (&optional (template-name "template"))
  (let* ((list (diary-list))
         (this-name (first list))
         (prev-name (second list))
         (this (dat-to-plist this-name))
         (prev nil)
         (diary (load-diary)))
    (when prev-name
      (setf prev (dat-to-plist prev-name))
      (setf (getf prev :next) this-name)
      (setf (getf this :prev) prev-name)
      (plist-to-dat prev-name prev t)
      (make-html prev-name template-name))
    (setf (getf diary :text)
          (format nil "* [~A](~A.htm)#~A~&~A"
                  this-name
                  this-name
                  (getf this :title)
                  (getf diary :text)))
    (setf (getf this :id) (funcall *gen-id*))
    (plist-to-dat this-name this t)
    (plist-to-dat "diary" diary t)
    (make-html this-name template-name)
    (make-html "diary" template-name)
    this))
    
(defun update-default-feed (name comment)
  (let ((this (dat-to-plist name)))
    (update-atom *feed-dat*
                 *feed-atom*
                 :title (getf this :title)
                 :link (get-url name)
                 :id (getf this :id)
                 :summary comment)))
  
(defun update-diary-with-feed (comment)
  (let* ((list (diary-list))
         (this-name (first list)))
    (update-diary)
    (update-default-feed this-name comment)))

