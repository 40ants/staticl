(uiop:define-package #:staticl/content/post
  (:use #:cl)
  (:import-from #:staticl/content
                #:content-from-file
                #:content-type)
  (:export #:post-type
           #:post))
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
