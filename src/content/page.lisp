(uiop:define-package #:staticl/content/page
  (:use #:cl)
  (:import-from #:staticl/content
                #:content-from-file
                #:content-type)
  (:export #:page-type
           #:page))
(in-package #:staticl/content/page)


(defclass page (content-from-file)
  ()
  (:default-initargs
   ;; In coleslaw page and post share the same template
   :template "post"))


(defclass page-type (content-type)
  ()
  (:default-initargs
   :type "page"
   :content-class 'page))
