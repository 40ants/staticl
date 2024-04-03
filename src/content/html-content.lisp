(uiop:define-package #:staticl/content/html-content
  (:use #:cl)
  (:export #:content-html
           #:content-html-excerpt))
(in-package #:staticl/content/html-content)


(defgeneric content-html (content)
  (:documentation "Returns a content as HTML string."))

(defgeneric content-html-excerpt (content)
  (:documentation "Returns an excerpt of full content as HTML string."))
