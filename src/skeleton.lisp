(uiop:define-package #:staticl/skeleton
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:import-from #:mystic.template.file
                #:file-mixin)
  (:import-from #:mystic
                #:make-option)
  (:import-from #:mystic.util
                #:read-template-file)
  (:import-from #:mystic.template.file
                #:file))
(in-package #:staticl/skeleton)


(defclass staticl-site (file-mixin)
  ()
  (:default-initargs
   :options (list (make-option :title
                               "Title"
                               "The site's title."
                               :requiredp t)
                  (make-option :description
                               "Description"
                               "The site's description."
                               :requiredp t)
                  (make-option :url
                               "URL"
                               "The site's URL."
                               :requiredp t))))


(defun read-all-files (from-dir)
  (loop with from-dir = (uiop:ensure-directory-pathname from-dir)
        for filename in (directory (uiop:wilden from-dir))
        for relative-path = (enough-namestring filename from-dir)
        unless (uiop:directory-pathname-p filename)
        collect
           (make-instance 'file
                          :path relative-path
                          :content (alexandria:read-file-into-string filename))))


(-> new-site ((or pathname string) string string &key (:description string))
    (values pathname &optional))

(defun new-site (path title url &key (description ""))
  "Creates a new site skeleton with a few posts."
  (let ((files
          (read-all-files
           (asdf:system-relative-pathname :staticl
                                          (make-pathname
                                           :directory '(:relative "skeleton")))))
        (full-output-dir
          (uiop:merge-pathnames*
           (uiop:ensure-directory-pathname path))))
    (mystic:render
     (make-instance 'staticl-site
                    :files files)
     (list :title title
           :url url
           :description description)
     full-output-dir)
    (values full-output-dir)))
