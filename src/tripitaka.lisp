;;;;
;;;;
;;;;

(in-package :tripitaka)

;;; Define special valiable
;;;
(defvar *rc-file* (merge-pathnames #p".tripitakarc"
                                          (user-homedir-pathname)))
(defvar *projects-plist* nil)
(defvar *project-dir* nil)
(defvar *home-dir* nil)
(defvar *dat-dir* nil)
(defvar *theme-dir* nil)
(defvar *project-file* nil)
(defvar *project* nil)
(defvar *feed-dat* nil)
(defvar *feed-atom* nil)
(defvar *gen-id* (lambda () nil))


;;; plist
;;;
(defvar *key-val-reg* "^:([a-zA-Z0-9$%&\\-+_~{}[\\]\\*?@]+)\\s+([^\\s].*)$")
(defvar *key-only-reg* "^:([a-zA-Z0-9$%&\\-+_~{}[\\]\\*?@]+)\\s*$")
(defvar *escape-colon-reg* "^:(:.*)$")

(defun key-val-line-p (line)
  (cl-ppcre:scan *key-val-reg* line))

(defun key-only-line-p (line)
  (cl-ppcre:scan *key-only-reg* line))

(defun escape-colon-line-p (line)
  (cl-ppcre:scan *escape-colon-reg* line))

(defun following-line-p (line)
  (not (or (key-val-line-p line)
           (key-only-line-p line)
           (escape-colon-line-p line))))

(defun reverse-plist (plist)
  (do* ((result nil)
        (pl plist (cddr pl))
        (key (car pl) (car pl))
        (val (cadr pl) (cadr pl)))
       ((not key) result)
    (push val result)
    (push key result)))

(defun read-prop-stream (in)
  (do ((plist nil)
       (sym nil)
       (line (read-line in nil nil) (read-line in nil nil)))
      ((not line) (reverse-plist plist))
    (cond ((key-val-line-p line) ;key and value
           (cl-ppcre:register-groups-bind (key val) (*key-val-reg* line)
             (setf sym (string-to-keyword key))
             (setf (getf plist sym) val)))
          ((key-only-line-p line) ;only key
           (cl-ppcre:register-groups-bind (key) (*key-only-reg* line)
             (setf sym (string-to-keyword key))
             (setf (getf plist sym) "")))
          ((escape-colon-line-p line) ;::hoge
           (when (keywordp sym)
               (cl-ppcre:register-groups-bind (val) (*escape-colon-reg* line)
                 (setf (getf plist sym) (format nil "~A~%~A" (getf plist sym) val)))))
          ((following-line-p line) ;following
           (when (keywordp sym)
               (setf (getf plist sym) (format nil "~A~%~A" (getf plist sym) line)))))))

(defun read-prop-file (filespec)
  (if-file-exists-do (filespec)
   (with-open-file (in filespec)
     (read-prop-stream in))))

(defun set-val (plist key val)
  (setf (getf plist (string-to-keyword key)) val))

(defun get-val (plist key)
  (let ((val (getf plist (string-to-keyword key))))
    (if (cl-ppcre:scan "^\\n" val)
        (subseq val 1)
        val)))

(defun get-val-as-list (plist key &key sep nth)
  (let* ((val (get-val plist key))
         (lst (cond (sep (cl-ppcre:split sep val))
                    ((cl-ppcre:scan "\\n" val) (cl-ppcre:split "\\n" val))
                    (t (cl-ppcre:split " " val)))))
    (if nth
        (nth nth lst)
        lst)))

(defun plist-to-string (plist)
  (with-open-stream (out (make-string-output-stream))
    (do* ((pl plist (cddr pl))
          (key (car pl) (car pl))
          (val (cadr pl) (cadr pl)))
         ((not key) (get-output-stream-string out))
      (format out "~&~S ~A" key val))))
        
(defun plist-to-file (plist filespec)
  (with-open-file (out filespec
                       :direction :output
                       :if-exists :supersede
                       :external-format *charset-utf8*)
    (format out "~A" (plist-to-string plist))))

(defun dat-to-plist (name)
  (read-prop-file (get-dat-path name)))

(defun plist-to-dat (plist name orveride)
  (if (and (not orveride) (probe-file (get-dat-path name)))
      (message (format nil "~a.dat exists." name))
      (progn
        (plist-to-file plist (get-dat-path name))
        t)))
      

;;; Initialize
;;;
(defun make-rc-file ()
  (plist-to-file (list :sample "~/tripitaka/sample/")
                 *rc-file*
                 nil)
  (message "~/.tripitakarc was created.")
  (when (make-dir (merge-pathnames #p"tripitaka/sample/" (user-homedir-pathname)))
    (make-project :dir (merge-pathnames #p"tripitaka/sample/" (user-homedir-pathname)))
    (message "~/tripitaka/sample/ was created."))
  (message "If you want to try sample, please copy sample/ directory to ~/tripitaka/sample/"))

(defun read-rc ()
  "read .tripitakarc"
  (unless (probe-file *rc-file*)
    (make-rc-file))
  (setf *projects-plist* (read-prop-file *rc-file*))
  (symbol-name (first *projects-plist*)))

(defun load-project ()
  "read config file"
  (setf *project* (read-prop-file *project-file*)))

(defun set-project (project)
  "set project config"
  (setf *project-dir*
        (cl-fad:pathname-as-directory (getf *projects-plist* (string-to-keyword project))))
  (setf *project-file* (merge-pathnames "project" *project-dir*))
  (unless (probe-file *project-file*)
    (make-project :name project))
  (setf *home-dir* (merge-pathnames "home/" *project-dir*))
  (setf *dat-dir* (merge-pathnames "dat/" *project-dir*))
  (setf *theme-dir* (merge-pathnames "theme/" *project-dir*))
  (setf *feed-dat* (merge-pathnames "feed" *project-dir*))
  (setf *feed-atom* (merge-pathnames "feed.xml" *home-dir*))
  (load-project)
  (message (format nil "~A is selected." project)))

(defun project (name)
  "get value from config"
  (getf *project* (string-to-keyword name)))

(defun set-id-func ()
  (setf *gen-id* (lambda () (gen-id-uuid-v4))))

(defun ready ()
  (set-project (read-rc))
  (set-id-func)
  (message "preparation is finished. This is developping Version"))

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

(defun diary-list ()
  (let ((flist (file-list "\\d{8}(-\\d+)?.dat$")))
    (if flist
        (sort flist #'diary>)
        (list ""))))

(defun get-url (name)
  (format nil "~A/~A.htm" (string-right-trim "/" (project "site-url")) name))


;;; Make new files
;;;
(defun make-project (&key (name nil) (dir nil))
  (let* ((pdir (if dir dir (getf *projects-plist* (string-to-keyword name))))
         (conf-file (merge-pathnames "project" pdir))
         (home-dir (merge-pathnames "home/" pdir))
         (theme-dir (merge-pathnames "theme/" pdir))
         (dat-dir (merge-pathnames "dat/" pdir)))
    (plist-to-file (list :site-name ""
                         :site-url ""
                         :author ""
                         :pubyear ""
                         :id "")
                   conf-file)
    (make-dir home-dir)
    (make-dir dat-dir)
    (make-dir theme-dir)))

(defun make-dat (name &key
                        (title "")
                        (date (get-date-string "-"))
                        (up "")
                        (next "")
                        (prev "")
                        (id "")
                        (text "")
                        (overwrite nil))
  (plist-to-dat (list :title title
                      :date date
                      :up up
                      :next next
                      :prev prev
                      :id id
                      :text text)
                name
                overwrite))


;;; make HTML String
;;;
;;;
(defun scan-template-type (tag)
  (let ((type-list (multiple-value-list (cl-ppcre:scan-to-strings "tri:([^\\s]+)" tag))))
    (aref (second type-list) 0)))

(defun scan-template-attr (tag)
    (let ((hash (make-hash-table :test #'equal)))
      (cl-ppcre:do-scans (start end r-start r-end "([^\\s=]+)\\s*=\\s*\"([^\\s=\"]+)\"" tag hash)
        (flet ((get-key-val (key-or-val) (subseq tag (aref r-start key-or-val) (aref r-end key-or-val))))
          (let ((key (get-key-val 0))
                (val (get-key-val 1)))
            (setf (gethash key hash) val))))))

(defun scan-template (target start-pos plist &optional (count 0))
  (let (type prop n name func result (attr (make-hash-table :test #'equal)))
    (multiple-value-bind (start end) 
        (cl-ppcre:scan "<tri:[^\\s]+(?:\\s+[^\\s=]+\\s*=\\s*\"[^\\s=\"]+\"|)+\\s*>" target :start start-pos)
      (cond (start
             (let ((tag (subseq target start end)))
               (setf attr (scan-template-attr tag))
               (setf type (scan-template-type tag))
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
                              (1+ count))))
            ((= count 0) target)
            (t (scan-template target 0 plist))))))

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
        (prev-name (first (diary-list))))
    (if (make-dat name :up "diary" :prev prev-name)
        (message (format nil "make ~A~A.dat" *dat-dir* name) t))))

(defun update-page (name &optional (template-name "template"))
  (let* ((page (dat-to-plist name))
         (id (getf page :id)))
    (if (or (not id) (equal "" id))
        (set-val page :id (funcall *gen-id*)))
    (plist-to-dat page name t)
    (make-html name template-name)))

(defun load-diary ()
  (unless (find-dat "diary")
    (make-dat "diary" :title "DIARY"))
  (dat-to-plist "diary"))

(defun add-diary (this-name this diary)
  (format nil "* [~A](~A.htm)#~A~&~A"
                  this-name
                  this-name
                  (get-val this :title)
                  (get-val diary :text)))

(defun update-diary (&optional (template-name "template"))
  (let* ((list (diary-list))
         (this-name (first list))
         (prev-name (second list))
         (this (dat-to-plist this-name))
         (prev nil)
         (diary (load-diary)))
    (when prev-name
      (setf prev (dat-to-plist prev-name))
      (set-val prev :next this-name)
      (set-val this :prev prev-name)
      (plist-to-dat prev prev-name t)
      (make-html prev-name template-name)
      (plist-to-dat this this-name t))
    (update-page this-name template-name)
    (set-val diary :text (add-diary this-name this diary))
    (plist-to-dat diary "diary" t)
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

