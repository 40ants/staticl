(uiop:define-package #:staticl
  (:use #:cl)
  (:import-from #:staticl/site
                #:site-content-root
                #:site-theme
                #:site-plugins
                #:site-url
                #:make-site)
  (:import-from #:staticl/content
                #:write-content
                #:read-contents
                #:preprocess)
  (:import-from #:serapeum
                #:->)
  (:import-from #:staticl/theme
                #:copy-static)
  (:import-from #:staticl/pipeline
                #:execute-pipeline)
  (:import-from #:staticl/current-root
                #:with-current-root)
  (:import-from #:staticl/url
                #:object-url
                #:with-base-url)
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
         ;; (initial-content (read-contents site))
         ;; (plugins (site-plugins site))
         ;; (additional-content
         ;;   (loop for plugin in plugins
         ;;         append (preprocess site plugin
         ;;                            initial-content)))
         ;; (all-content (append initial-content
         ;;                      additional-content))
         (all-content (execute-pipeline site))
         )
    
    (with-current-root ((site-content-root site))
      (with-base-url ((site-url site))
        (loop for content in all-content
              do (write-content site content stage-dir))))

    (copy-static (site-theme site)
                 stage-dir)
    (values)))
