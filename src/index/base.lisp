(uiop:define-package #:staticl/index/base
  (:use #:cl)
  (:import-from #:serapeum
                #:dict
                #:soft-list-of)
  (:import-from #:staticl/content
                #:path-matches-p
                #:has-tag-p
                #:content-template
                #:write-content-to-stream
                #:get-target-filename
                #:content)
  (:import-from #:staticl/site
                #:site)
  (:import-from #:staticl/theme
                #:template-vars)
  (:import-from #:staticl/url
                #:object-url)
  (:import-from #:staticl/current-root
                #:current-root)
  (:import-from #:staticl/injections
                #:content-with-injections-mixin)
  (:export #:index-page
           #:page-items
           #:prev-page
           #:next-page
           #:base-index
           #:index-target-path
           #:page-size
           #:page-target-path
           #:page-title))
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


(defmethod shared-initialize ((instance base-index) slot-names &rest initargs &key &allow-other-keys)
  (when (getf initargs :target-path)
    (setf (getf initargs :target-path)
          (pathname (getf initargs :target-path))))
  (apply #'call-next-method instance slot-names initargs))


(defclass index-page (content-with-injections-mixin content)
  ((target-path :initarg :target-path
                :type pathname
                :documentation "Relative pathname to a file with page content."
                :reader page-target-path)
   (title :initarg :title
          :type string
          :documentation "A title of the page."
          :reader page-title)
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
   :items nil
   :target-path (error "TARGET-PATH is required argument for an index page.")
   :title (error "TITLE is required argument for an index page.")
   :template *default-template*))


(defmethod get-target-filename ((site site) (page index-page) stage-dir &key make-clean-if-needed)
  (declare (ignore make-clean-if-needed))
  (merge-pathnames (page-target-path page)
                   stage-dir))



(defmethod template-vars ((site site) (content index-page) &key (hash (dict)))
  (flet ((item-vars (item)
           (dict "url"
                 (staticl/url:object-url site item)
                 "title"
                 (staticl/content:content-title item)
                 "created-at"
                 (staticl/content:content-created-at item)
                 "excerpt"
                 (staticl/content/html-content:content-html-excerpt
                  site
                  item
                  content)
                 "has-more"
                 (staticl/content/html-content:has-more-content-p item))))
    (declare (dynamic-extent #'item-vars))
    
    (setf (gethash "title" hash)
          (page-title content)
          (gethash "items" hash)
          (mapcar #'item-vars (page-items content))
          (gethash "prev" hash)
          (when (prev-page content)
            (dict
             "url"
             (staticl/url:object-url site (prev-page content))))
          (gethash "next" hash)
          (when (next-page content)
            (dict
             "url"
             (staticl/url:object-url site (next-page content))))))
  
  (if (next-method-p)
      (call-next-method site content :hash hash)
      (values hash)))


(defmethod object-url ((site site) (index index-page) &key &allow-other-keys)
  (let* ((root (current-root))
         (relative-path (enough-namestring (page-target-path index)
                                           root)))
    (uiop:unix-namestring relative-path)))


(defmethod has-tag-p ((index index-page) (tag-name string))
  "For index pages this method will return T if at least one content item on the page has required tag name."
  (loop for item in (page-items index)
        thereis (has-tag-p item tag-name)))


(defmethod path-matches-p ((content index-page) (path pathname))
  (let ((full-target-path (merge-pathnames (page-target-path content)
                                           (current-root))))
    (when (uiop:subpathp full-target-path
                         path)
      (values t))))
