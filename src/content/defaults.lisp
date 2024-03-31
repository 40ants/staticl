(uiop:define-package #:staticl/content/defaults
  (:use #:cl)
  (:import-from #:staticl/content
                #:supported-content-types)
  (:import-from #:staticl/site
                #:site)
  (:import-from #:staticl/content/page
                #:page-type))
(in-package #:staticl/content/defaults)


(defmethod supported-content-types ((site site))
  (list (make-instance 'page-type)))
