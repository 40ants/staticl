(uiop:define-package #:staticl/content/file
  (:use #:cl)
  (:import-from #:staticl/plugins/sitemap
                #:should-be-included-into-sitemap-p)
  (:import-from #:staticl/content
                #:get-target-filename
                #:write-content)
  (:import-from #:staticl/site
                #:site-content-root
                #:site)
  (:import-from #:serapeum
                #:absolute-pathname)
  (:export #:file
           #:file-path))
(in-package #:staticl/content/file)


(defclass file ()
  ((path :initarg :path
         :type absolute-pathname
         :reader file-path))
  (:documentation "Represents a file on the disk. Usually this will be an image or some other asset which should be published along the main content."))


(defmethod should-be-included-into-sitemap-p ((file file))
  (values nil))


(defmethod get-target-filename ((site site) (file file) (stage-dir pathname) &key make-clean-if-needed)
  (declare (ignore make-clean-if-needed))
  
  (let ((relative-path (enough-namestring (file-path file)
                                          (site-content-root site))))
    (merge-pathnames relative-path
                     stage-dir)))


(defmethod write-content ((site site) (file file) (stage-dir pathname))
  (let* ((target-filename (get-target-filename site file stage-dir)))
    (ensure-directories-exist target-filename)

    (uiop:copy-file (file-path file)
                    target-filename)
    (values)))
