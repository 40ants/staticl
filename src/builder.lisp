(uiop:define-package #:staticl/builder
  (:use #:cl)
  (:import-from #:staticl/site
                #:site-content-root
                #:site-theme
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
                #:with-base-url))
(in-package #:staticl/builder)


(-> generate (&key
              (:root-dir (or pathname string))
              (:stage-dir (or pathname string)))
    (values pathname &optional))

(defun generate (&key
                 (root-dir *default-pathname-defaults*)
                 (stage-dir (merge-pathnames (make-pathname :directory '(:relative "stage"))
                                             (uiop:ensure-directory-pathname root-dir))))
  (let* ((root-dir
           ;; Here we ensure both root and stage dirs are absolute and point to the directories
           (merge-pathnames
            (uiop:ensure-directory-pathname root-dir)))
         (stage-dir
           (merge-pathnames
            (uiop:ensure-directory-pathname stage-dir)))
         (site (make-site root-dir)))
    (with-current-root ((site-content-root site))
      (with-base-url ((site-url site))
        (loop with all-content = (execute-pipeline site)
              for content in all-content
              do (write-content site content stage-dir))))

    (copy-static (site-theme site)
                 stage-dir)
    (values stage-dir)))
