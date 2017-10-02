(in-package :tripitaka)

;; tripitaka::functions

(deftrifun get-prop (name prop) 
  (get-prop name prop))

(deftrifun get-prop-as-list (name prop) 
  (get-prop-as-list name prop))

(deftrifun dolist (list func)
  (format nil "~{~A~}" (mapcar func list)))

(deftrifun anchor (href label)
  (make-html (list :a (list :href href) label)))

