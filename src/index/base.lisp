(uiop:define-package #:staticl/index/base
  (:use #:cl)
  (:import-from #:serapeum
                #:dict
                #:soft-list-of)
  (:import-from #:staticl/content
                #:content-template
                #:write-content-to-stream
                #:get-target-filename
                #:content)
  (:import-from #:staticl/site
                #:site)
  (:import-from #:staticl/theme
                #:template-vars)
  (:export #:index-page
           #:page-items
           #:prev-page
           #:next-page
           #:index-page-template
           #:base-index
           #:index-target-path
           #:page-size
           #:page-target-path))
(in-package #:staticl/index/base)


(defvar *default-target-path* (make-pathname :directory '(:relative ".")))

(defvar *default-page-size* 10)

(defvar *default-template* "index")



(defclass base-index ()
  ((target-path :initarg :target-path
                :type pathname
                :documentation "Relative pathname to a directory where all pages will be generated."
                :reader index-target-path)
   (page-size :initarg :page-size
              :type integer
              :reader page-size)
   (template :initarg :template
             :type string
             :reader content-template))
  (:default-initargs
   :target-path *default-target-path*
   :page-size *default-page-size*
   :template *default-template*))


(defclass index-page (content)
  ((target-path :initarg :target-path
                :type pathname
                :documentation "Relative pathname to a file with page content."
                :reader page-target-path)
   (items :initarg :items
          :type (soft-list-of content)
          :reader page-items)
   (prev-page :initarg :prev-page
              :type (or null index-page)
              :accessor prev-page)
   (next-page :initarg :next-page
              :type (or null index-page)
              :accessor next-page)
   (template :initarg :template
             :type string
             :reader content-template))
  (:default-initargs
   :prev-page nil
   :next-page nil
   :template *default-template*))


(defmethod get-target-filename ((site site) (page index-page) stage-dir)
  (merge-pathnames (page-target-path page)
                   stage-dir))



(defmethod template-vars ((content index-page) &key (hash (dict)))
  (flet ((item-vars (content)
           (dict "url"
                 (staticl/url:object-url content)
                 "title"
                 (staticl/content:content-title content)
                 "created-at"
                 (staticl/content:content-created-at content)
                 "excerpt"
                 (staticl/content/html-content:content-html-excerpt content))))
    (declare (dynamic-extent #'item-vars))
    
    (setf (gethash "title" hash)
          "Page N"
          (gethash "items" hash)
          (mapcar #'item-vars (page-items content))
          (gethash "prev" hash)
          (when (prev-page content)
            (dict
             "url"
             (staticl/url:object-url (prev-page content))))
          (gethash "next" hash)
          (when (next-page content)
            (dict
             "url"
             (staticl/url:object-url (next-page content))))))
  
  (if (next-method-p)
      (call-next-method content :hash hash)
      (values hash)))
