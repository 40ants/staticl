(uiop:define-package #:staticl/injections
  (:use #:cl)
  (:import-from #:serapeum
                #:->
                #:dict)
  (:import-from #:log)
  (:import-from #:staticl/theme
                #:template-vars)
  (:import-from #:staticl/site
                #:site)
  (:export #:content-injections
           #:content-with-injections-mixin
           #:add-injection))
(in-package #:staticl/injections)


(defclass content-with-injections-mixin ()
  ((injections :initform (dict)
               :reader content-injections)))


(defgeneric add-injection (content point-name html)
  (:documentation "Adds a piece of HTML to the list of pieces to be inserted to a given point when content will be rendered to a file.")
  
  (:method ((content t) (point-name string) (html string))
    (log:warn "Injections are not supported by content of type ~S"
              (class-name (class-of content)))
    (values))
  
  (:method ((content content-with-injections-mixin) (point-name string) (html string))
    (push html
          (gethash point-name
                   (slot-value content 'injections)))
    (values)))


(defmethod template-vars ((site site) (content content-with-injections-mixin) &key (hash (dict)))
  (setf (gethash "injections" hash)
        (content-injections content))
  
  (if (next-method-p)
      (call-next-method site content :hash hash)
      (values hash)))
