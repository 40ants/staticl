(uiop:define-package #:staticl/index/paginated
  (:use #:cl)
  (:import-from #:staticl/pipeline)
  (:import-from #:staticl/site
                #:site)
  (:import-from #:staticl/index/base
                #:page-size
                #:index-target-path
                #:index-page
                #:base-index)
  (:import-from #:serapeum
                #:fmt)
  (:export #:paginated-index
           #:make-page-title
           #:make-page-filename))
(in-package #:staticl/index/paginated)


(defclass paginated-index (base-index)
  ())


(defun paginated-index (&rest initargs &key target-path page-size template)
  (declare (ignore target-path page-size template))
  (apply #'make-instance 'paginated-index
         initargs))


(defgeneric make-page-filename (index page-number)
  (:documentation "Should return a relative pathname like 2.html or page-2.html.")
  (:method ((index paginated-index) page-number)
    (let ((name (cond
                  ((= page-number 1)
                   "index")
                  (t
                   (fmt "page-~A" page-number)))))
      (make-pathname :name name
                     :type "html"))))


(defgeneric make-page-title (index page-number)
  (:documentation "Should return a string with a title for the page")
  (:method ((index paginated-index) page-number)
    (fmt "Page ~A" page-number)))


(defmethod staticl/pipeline:process-items ((site site) (index paginated-index) content-items)
  (loop for batch in (serapeum:batches content-items (page-size index))
        for page-number upfrom 1
        collect (make-instance 'index-page
                               :title (make-page-title index page-number)
                               :target-path (merge-pathnames
                                             ;; TODO: implement clean urls
                                             (make-page-filename index page-number)
                                             (uiop:ensure-directory-pathname
                                              (index-target-path index)))
                               :items batch) into pages
        finally (loop for (prev page next) on (list* nil pages)
                      when page
                        do (setf (staticl/index/base:prev-page page)
                                 prev
                                 (staticl/index/base:prev-page page)
                                 next)
                           (staticl/pipeline:produce-item page)))
  (values))
