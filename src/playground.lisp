(uiop:define-package #:staticl/playground
  (:use #:cl))
(in-package #:staticl/playground)


(defclass site ()
  ())


(defclass my-site (site)
  ())


(defclass content ()
  ())

(defclass post (content)
  ())


(defclass multilingual-post (post)
  ())


;; (defgeneric write-content (site content)
;;   (:method (site content)
;;     (log:info "Called (T T) method"))
  
;;   (:method ((site site) (content post))
;;     (log:info "Called (SITE POST) method"))

;;   (:method ((site my-site) content)
;;     (log:info "Called (MY-SITE T) method")))

(defgeneric write-content (site content)
  (:method (site content)
    (log:info "Called (T T) method"))
  
  (:method ((site site) (content post))
    (log:info "Called (SITE POST) method"))

  (:method ((site site) (content multilingual-post))
    (log:info "Called (SITE MULTILINGUAL-POST) method")))
