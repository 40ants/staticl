(uiop:define-package #:staticl/content/html-content
  (:use #:cl)
  (:export #:content-html
           #:content-html-excerpt
           #:has-more-content-p))
(in-package #:staticl/content/html-content)


(defgeneric content-html (content)
  (:documentation "Returns a content as HTML string."))

(defgeneric content-html-excerpt (content)
  (:documentation "Returns an excerpt of full content as HTML string."))

(defgeneric has-more-content-p (content)
  (:documentation "Returns T if there is more content than was returned by CONTENT-HTML-EXCERPT generic-function."))
