(uiop:define-package #:staticl/links/link
  (:use #:cl)
  (:import-from #:staticl/content
                #:content-title
                #:content)
  (:import-from #:serapeum
                #:dict
                #:dict*
                #:->)
  (:import-from #:staticl/url
                #:object-url)
  (:export
   #:link))
(in-package #:staticl/links/link)


(defclass link ()
  ((content :initarg :content
            :type content
            :reader link-content)))


(-> link (content)
    (values link &optional))

(defun link (content)
  "Creates a link to the given content piece.

   When such object is passed to the template, it is resolved to a
   page URL and title."
  (make-instance 'link
                 :content content))


(defmethod staticl/theme:template-vars ((link link) &key (hash (dict)))
  (dict* hash
         "url"
         (object-url (link-content link))
         "title"
         (content-title (link-content link))))
