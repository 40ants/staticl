(uiop:define-package #:staticl/content-pipeline
  (:use #:cl)
  (:import-from #:staticl/pipeline)
  (:import-from #:staticl/content)
  (:import-from #:staticl/site
                #:site)
  (:import-from #:serapeum
                #:soft-list-of
                #:->)
  (:export #:load-content
           #:exclude-patterns))
(in-package #:staticl/content-pipeline)


(defclass load-content ()
  ((exclude :initarg :exclude
            :reader exclude-patterns)))


(-> load-content (&key (:exclude (soft-list-of string)))
    (values load-content &optional))

(defun load-content (&key (exclude (list ".qlot")))
  "Reads content from the disk.

   By default only `post` and `page` files are loaded. But this list could be extended if you'll define
   a custom site class and a method for STATICL/CONTENT:SUPPORTED-CONTENT-TYPES generic-function."
  (make-instance 'load-content
                 :exclude exclude))


(defmethod staticl/pipeline:process-items ((site site) (node load-content) content-items)
  (loop for new-item in (staticl/content:read-contents site
                                                       :exclude (exclude-patterns node))
        do (staticl/pipeline:produce-item new-item)))
