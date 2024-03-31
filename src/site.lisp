(uiop:define-package #:staticl/site
  (:use #:cl)
  (:import-from #:log)
  (:import-from #:serapeum
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
  (:export
   #:site
   #:site-content-root
   #:site-title
   #:make-site
   #:site-plugins
   #:site-theme))
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
   (theme :initarg :theme
          :type theme
          :reader site-theme
          :documentation "A theme object for the site.")
   (plugins :initarg :plugins
            :type (soft-list-of plugin)
            :reader site-plugins
            :documentation "A list of site plugins."))
  (:default-initargs
   :theme "hyde"
   :root (error "ROOT argument is required.")))


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
                                        (serapeum:mapply #'make-plugin
                                                         value)))))))


(-> make-site (pathname)
    (values site &optional))

(defun make-site (root)
  (let* ((root (probe-file (uiop:ensure-directory-pathname root)))
         (rc-file (merge-pathnames #P".staticlrc"
                                   root)))
    (unless (probe-file rc-file)
      (error "File ~A was not found."
             rc-file))
    (let* ((args (uiop:read-file-form rc-file))
           (depends-on (getf args :depends-on))
           (class-name (getf args :class
                             "staticl/site:site"))
           (site-plugins-dir (merge-pathnames (make-pathname :directory '(:relative "plugins"))
                                              root)))

      (when depends-on
        (mapcar #'load-system depends-on))

      (load-files-from site-plugins-dir)

      ;; We should load class only after all
      ;; dependencies were loaded
      (let ((class (find-class-by-name class-name)))
      
        (apply #'make-instance class
               :root root
               args)))))


(defmethod template-vars ((site site) &key (hash (dict)))
  (setf (gethash "title" hash)
        (site-title site)))
