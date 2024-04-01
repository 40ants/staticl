(uiop:define-package #:staticl/tag
  (:use #:cl)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:staticl/theme
                #:template-vars)
  (:export #:tag-name
           #:tag))
(in-package #:staticl/tag)


(defclass tag ()
  ((name :initarg :name
         :type string
         :reader tag-name))
  (:default-initargs
   :name (error ":NAME is required argument for a tag.")))


(defmethod template-vars ((tag tag) &key (hash (dict)))
  (setf (gethash "name" hash)
        (tag-name tag))
  (values hash))
