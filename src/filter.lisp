(uiop:define-package #:staticl/filter
  (:use #:cl)
  (:import-from #:serapeum
                #:directory-pathname)
  (:import-from #:staticl/site
                #:site)
  (:export #:filter))
(in-package #:staticl/filter)


(defclass filter ()
  ((path :initarg :path
         :type (or null directory-pathname)
         :reader filter-path)
   (invert :initarg :invert
           :type boolean
           :reader filter-invert)
   (pipeline :initarg :pipeline
             :type list
             :reader pipeline-items))
  (:default-initargs
   :path nil
   :invert nil
   :pipeline nil))


(defmacro filter ((&key path invert) &rest pipeline)
  (alexandria:once-only (path)
    `(make-instance 'filter
                    :path (when ,path
                            (uiop:ensure-directory-pathname ,path))
                    :invert ,invert
                    :pipeline (list ,@pipeline))))


(defmethod staticl/pipeline:process-items ((site site) (node filter) content-items)
  (let ((filtered content-items))
    (loop for subnode in (pipeline-items node)
          do (staticl/pipeline:process-items site subnode filtered))))
