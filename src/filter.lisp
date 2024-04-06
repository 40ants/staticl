(uiop:define-package #:staticl/filter
  (:use #:cl)
  (:import-from #:serapeum
                #:soft-list-of
                #:->
                #:directory-pathname)
  (:import-from #:staticl/site
                #:site)
  (:import-from #:staticl/content
                #:content)
  (:export #:filter))
(in-package #:staticl/filter)


(deftype filter-predicate ()
  '(function (content)
    (values boolean &optional)))


(defclass filter ()
  ((filter-fn :initarg :filter-fn
              :type filter-predicate
              :reader filter-fn)
   (pipeline :initarg :pipeline
             :type list
             :reader pipeline-items))
  (:default-initargs
   :pipeline nil))


(defmacro filter ((&key path invert) &rest pipeline)
  "Filters input content objects and processes them using steps given as a body.

   Arguments:

   - PATH: if given result will contain only items read from the given path.
   - INVERT: inverts effect of the filter."
  (alexandria:once-only (path)
    (let (rules)

      (when path
        (push `(path-matches-p item (uiop:ensure-directory-pathname ,path))
              rules))

      (setf rules
            (if invert
                `(not (and ,@rules))
                `(and ,@rules)))
      
      
      `(flet ((filter-fn (item)
                ,rules))
         (make-instance 'filter
                        :filter-fn #'filter-fn
                        :pipeline (list ,@pipeline))))))


(-> filter-items ((soft-list-of content) filter)
    (soft-list-of content))

(defun filter-items (content-items filter-node)
  (remove-if-not (filter-fn filter-node)
                 content-items))


(defmethod staticl/pipeline:process-items ((site site) (node filter) content-items)
  (let ((filtered (filter-items content-items node)))
    (loop for subnode in (pipeline-items node)
          do (staticl/pipeline:process-items site subnode filtered))))


(-> path-matches-p (content pathname)
    (values boolean &optional))

(defun path-matches-p (content path)
  (when (typep content 'staticl/content:content-from-file)
    (uiop:subpathp (staticl/content:content-file content)
                   path)))
