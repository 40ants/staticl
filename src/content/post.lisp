(uiop:define-package #:staticl/content/post
  (:use #:cl)
  (:import-from #:staticl/content
                #:content-from-file
                #:content-type)
  (:import-from #:serapeum
                #:->)
  (:export #:post-type
           #:post
           #:postp))
(in-package #:staticl/content/post)


(defclass post (content-from-file)
  ()
  (:default-initargs
   ;; In coleslaw page and post share the same template
   :template "post"))


(defclass post-type (content-type)
  ()
  (:default-initargs
   :type "post"
   :content-class 'post))


(-> postp (t)
    (values boolean &optional))

(defun postp (content-item)
  "Returns T if given object is a content of type POST."
  (typep content-item 'post))
