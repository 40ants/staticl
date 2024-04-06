(uiop:define-package #:staticl/site
  (:use #:cl)
  (:import-from #:log)
  (:import-from #:local-time)
  (:import-from #:serapeum
                #:dict*
                #:directory-pathname
                #:dict
                #:soft-list-of
                #:->)
  (:import-from #:staticl/utils
                #:load-files-from
                #:normalize-plist
                #:find-class-by-name
                #:load-system)
  (:import-from #:staticl/plugin
                #:make-plugin
                #:plugin)
  (:import-from #:staticl/theme
                #:template-vars
                #:load-theme
                #:theme)
  (:import-from #:staticl/url
                #:assert-absolute-url
                #:object-url)
  (:import-from #:staticl/current-root
                #:with-current-root
                #:current-root)
  (:import-from #:staticl/navigation
                #:menu)
  (:export #:site
           #:site-content-root
           #:site-title
           #:make-site
           #:site-plugins
           #:site-theme
           #:site-pipeline
           #:site-description))
(in-package #:staticl/site)


(defclass site ()
  ((root :initarg :root
         :type pathname
         :reader site-content-root
         :documentation "A directory pathname where .staticlrc file can be found.")
   (title :initarg :title
          :type string
          :reader site-title
          :documentation "Site's title.")
   (description :initarg :title
                :type string
                :reader site-description
                :documentation "Site's description.")
   (navigation :initarg :navigation
               :type (or null menu)
               :reader site-navigation
               :documentation "Site's navigation.")
   (charset :initarg :charset
            :type string
            :reader site-charset
            :documentation "Site's charset. By default it is UTF-8.")
   (url :initarg :url
        :type string
        :reader site-url
        :documentation "Site's URL.")
   (theme :initarg :theme
          :type theme
          :reader site-theme
          :documentation "A theme object for the site.")
   (pipeline :initarg :pipeline
             ;; :type (soft-list-of plugin)
             :reader site-pipeline
             :documentation "A list of pipline nodes"))
  (:default-initargs
   :theme "hyde"
   :root (error "ROOT argument is required.")
   :description (error "DECRIPTION argument is required.")
   :navigation nil
   :url (error "URL argument is required.")
   :charset "UTF-8"))


(defun site (title &rest args)
  (when (getf args :url)
    (assert-absolute-url (getf args :url)))
  
  (apply #'make-instance 'site
         :title title
         :root (probe-file (current-root))
         args))


(defmethod print-object ((obj site) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S :root ~S"
            (site-title obj)
            (site-content-root obj))))


(defmethod initialize-instance :around ((obj site) &rest initargs &key root &allow-other-keys)
  (apply #'call-next-method
         obj
         (normalize-plist initargs
                          :theme (lambda (value)
                                   (etypecase value
                                     (string
                                      (load-theme value
                                                  :site-root root))))
                          :plugins (lambda (value)
                                     (etypecase value
                                       (list
                                        (loop for item in value
                                              collect (typecase item
                                                        ;; Here we support specifying a plugin
                                                        ;; as a plist with a :class attribute.
                                                        (list (apply #'make-plugin
                                                                     value))
                                                        (t item)))))))))


(-> make-site (pathname)
    (values site &optional))

(defun make-site (root)
  (let* ((root (probe-file (uiop:ensure-directory-pathname root)))
         (rc-file (merge-pathnames #P".staticlrc"
                                   root)))
    (unless (probe-file rc-file)
      (error "File ~A was not found."
             rc-file))
    
    (let ((site-plugins-dir (merge-pathnames (make-pathname :directory '(:relative "plugins"))
                                             root)))
      (load-files-from site-plugins-dir))
    
    (let ((site (let ((*package* (find-package "STATICL-USER")))
                  (with-current-root (root)
                    (uiop:eval-input rc-file)))))


      (unless (typep site 'site)
        (error "Config ~S does not return a SITE object."
               rc-file))
      (values site))))


(defmethod template-vars ((site site) &key (hash (dict)))
  (dict* hash 
         "title"
         (site-title site)
         "description" 
         (site-description site)
         "url" 
         (site-url site)
         "pubdate" 
         (local-time:now)
         "charset"
         (site-charset site)
         "navigation"
         (site-navigation site)))


(defmethod object-url ((site site) &key &allow-other-keys)
  (site-url site))
