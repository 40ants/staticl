(uiop:define-package #:staticl
  (:use #:cl)
  (:import-from #:staticl/site
                #:site-theme
                #:site-plugins
                #:make-site)
  (:import-from #:staticl/content
                #:write-content
                #:read-contents
                #:preprocess)
  (:import-from #:serapeum
                #:->)
  (:import-from #:staticl/theme
                #:copy-static)
  (:nicknames #:staticl/core)
  (:export #:generate
           #:stage))
(in-package #:staticl)


(-> stage (&key
           (:root-dir pathname)
           (:stage-dir pathname))
    (values &optional))

(defun stage (&key
                (root-dir *default-pathname-defaults*)
                (stage-dir (merge-pathnames (make-pathname :directory '(:relative "stage"))
                                            (uiop:ensure-directory-pathname root-dir))))
  (let* ((site (make-site root-dir))
         (initial-content (read-contents site))
         (plugins (site-plugins site))
         (additional-content
           (loop for plugin in plugins
                 append (preprocess site plugin
                                    initial-content)))
         (all-content (append initial-content
                              additional-content)))
    (loop for content in all-content
          do (write-content site content stage-dir))

    (copy-static (site-theme site)
                 stage-dir)
    (values)))
