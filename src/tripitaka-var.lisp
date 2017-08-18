(in-package :tripitaka)

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
