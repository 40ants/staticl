(uiop:define-package #:staticl/links/prev-next
  (:use #:cl)
  (:import-from #:staticl/pipeline)
  (:import-from #:serapeum)
  (:import-from #:staticl/content
                #:set-metadata)
  (:import-from #:staticl/site
                #:site)
  (:import-from #:staticl/links/link
                #:link)
  (:import-from #:staticl/content/post
                #:postp)
  (:export #:prev-next-links))
(in-package #:staticl/links/prev-next)


(defclass prev-next-links ()
  ())


(defun prev-next-links ()
  "Creates a links between pages."
  (make-instance 'prev-next-links))


(defmethod staticl/pipeline:process-items ((site site) (node prev-next-links) content-items)
  (loop with only-posts = (remove-if-not #'postp content-items)
        for (prev item next) on (list* nil only-posts)
        when item
          do (when prev
               (set-metadata item "prev"
                             (link prev)))
             (when next
               (set-metadata item "next"
                             (link next))))
  (values))
