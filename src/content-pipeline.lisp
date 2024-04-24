(uiop:define-package #:staticl/content-pipeline
  (:use #:cl)
  (:import-from #:staticl/pipeline)
  (:import-from #:staticl/content)
  (:import-from #:staticl/site
                #:site)
  (:export #:load-content))
(in-package #:staticl/content-pipeline)


(defclass load-content ()
  ())


(defun load-content ()
  (make-instance 'load-content))


(defmethod staticl/pipeline:process-items ((site site) (node load-content) content-items)
  (loop for new-item in (staticl/content:read-contents site)
        do (staticl/pipeline:produce-item new-item)))
