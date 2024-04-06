(uiop:define-package #:staticl/navigation
  (:use #:cl)
  (:import-from #:serapeum
                #:soft-list-of
                #:->)
  (:export #:menu
           #:menu-items
           #:item
           #:menu-item-url
           #:menu-item-title))
(in-package #:staticl/navigation)


(defclass item ()
  ((url :initarg :url
         :type string
         :reader menu-item-url)
   (title :initarg :title
           :type string
           :reader menu-item-title)))


(-> item (string string)
    (values item &optional))

(defun item (title url)
  (make-instance 'item
                 :title title
                 :url url))


(defclass menu ()
  ((items :initarg :items
          :type (soft-list-of (or item menu))
          :reader menu-items)))


(-> menu (&rest (or item menu))
    (values menu &optional))

(defun menu (&rest items)
  (make-instance 'menu
                 :items items))

