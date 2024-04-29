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
  (:import-from #:staticl/content/post
                #:postp)
  (:export #:paginated-index))
(in-package #:staticl/index/paginated)


(deftype function-from-int-to-string ()
  '(function (integer) (values string &optional)))


(deftype function-from-int-to-pathname ()
  '(function (integer) (values pathname &optional)))

(declaim (ftype function-from-int-to-string
                default-page-title-fn))

(defun default-page-title-fn (page-number)
  (fmt "Page ~A" page-number))


(declaim (ftype function-from-int-to-pathname
                default-page-filename-fn))

(defun default-page-filename-fn (page-number)
  (let ((name (cond
                ((= page-number 1)
                 "index")
                (t
                 (fmt "page-~A" page-number)))))
    (make-pathname :name name
                   :type "html")))


(defclass paginated-index (base-index)
  ((page-filename-fn :initarg :page-filename-fn
                     :type (or null function-from-int-to-pathname)
                     :documentation "A callback to change page titles.

                                     Accepts single argument - a page number and should return a pathname relative to the site's root.
                                     By default, it returns index.html for the first page and page-2.html, page-3.html for others.

                                     If site has \"clean urls\" setting enabled, then additional transformation to the pathname will be
                                     applied automatically."
                     :reader page-filename-fn)
   (page-title-fn :initarg :page-title-fn
                  :type (or null function-from-int-to-string)
                  :documentation "A callback to change page titles.

                                  Accepts single argument - a page number and should return a string.

                                  For example, here is how you can translate page title into a russian:

                                  ```lisp
                                  (paginated-index :target-path #P\"ru/\"
                                                   :page-title-fn (lambda (num)
                                                                    (fmt \"Страница ~A\" num)))
                                  ```
                                 "
                  :reader page-title-fn))
  (:default-initargs
   :page-title-fn #'default-page-title-fn
   :page-filename-fn #'default-page-filename-fn))


(defun paginated-index (&rest initargs &key target-path page-size template page-title-fn page-filename-fn)
  (declare (ignore target-path page-size template page-title-fn page-filename-fn))
  (apply #'make-instance 'paginated-index
         initargs))


(defmethod staticl/pipeline:process-items ((site site) (index paginated-index) content-items)
  (loop with only-posts = (remove-if-not #'postp content-items)
        with sorted-posts = (sort only-posts
                                  #'local-time:timestamp>
                                  :key #'staticl/content:content-created-at )
        for batch in (serapeum:batches sorted-posts (page-size index))
        for page-number upfrom 1
        collect (make-instance 'index-page
                               :title (funcall (page-title-fn index)
                                               page-number)
                               :target-path (merge-pathnames
                                             ;; TODO: implement clean urls
                                             (funcall (page-filename-fn index)
                                                      page-number)
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
