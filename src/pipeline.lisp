(uiop:define-package #:staticl/pipeline
  (:use #:cl)
  (:import-from #:staticl/site
                #:site
                #:site-pipeline)
  (:import-from #:serapeum
                #:soft-list-of
                #:->)
  (:import-from #:staticl/content
                #:content)
  (:export #:execute-pipeline
           #:process-items
           #:produce-item
           #:remove-item))
(in-package #:staticl/pipeline)


(defgeneric process-items (site pipeline-node content-items)
  (:documentation "A method for this generic function should process CONTENT-ITEMS - a list of conten items
                   produced by a previous pipeline nodes.

                   During the execution, method can call PRODUCE-ITEM or REMOVE-ITEM functions to add a new content
                   or to remove some content item."))

(defvar *produce-item-func*)

(defun produce-item (item)
  (funcall *produce-item-func* item))


(defvar *remove-item-func*)

(defun remove-item (item)
  (funcall *remove-item-func* item))


(-> execute-pipeline (site &key (:alter-pipeline function))
    (values (soft-list-of content)))

(defun execute-pipeline (site &key (alter-pipeline #'identity))
  (let ((known-items nil)
        (items-to-remove nil))
    (flet ((produce-item-func (item)
             (push item known-items)
             (values))
           (remove-item-func (item)
             ;; Items to remove are pushed into an intermediate list
             ;; just because there may be some siteeffects if we'll
             ;; modify KNOWN-ITEMS list while iterating on it.
             (push item items-to-remove)
             (values)))
      (declare (dynamic-extent #'produce-item-func
                               #'remove-item-func))
      
      (let ((*produce-item-func* #'produce-item-func)
            (*remove-item-func* #'remove-item-func))
        (loop for pipeline-node in (funcall alter-pipeline
                                            (site-pipeline site))
              do (process-items site pipeline-node known-items)
                 (when items-to-remove
                   ;; This is N*M complexity, but length of items-to-remove
                   ;; usually should be relatively small.
                   ;; It is possible to optimize this in future, for example
                   ;; by remembering an index of the items to remove instead
                   ;; of the item itself.
                   (setf known-items
                         (remove-if (lambda (item)
                                      (member item items-to-remove))
                                    known-items))
                   (setf items-to-remove nil)))
        (values known-items)))))
