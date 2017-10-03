(in-package :tripitaka)

;; tripitaka::functions

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
