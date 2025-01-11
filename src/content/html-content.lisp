(uiop:define-package #:staticl/content/html-content
  (:use #:cl)
  (:export #:content-html
           #:content-html-excerpt
           #:has-more-content-p))
(in-package #:staticl/content/html-content)


(defgeneric content-html (site content relative-to-content &key absolute-urls)
  (:documentation "Returns a content as HTML string.

                   If ABSOLUTE-URLS argument is T, then links to media files will be rewritten
                   to be absolute, starting from https://."))

(defgeneric content-html-excerpt (site content relative-to-content &key absolute-urls)
  (:documentation "Returns an excerpt of full content as HTML string.

                   If ABSOLUTE-URLS argument is T, then links to media files will be rewritten
                   to be absolute, starting from https://."))

(defgeneric has-more-content-p (content)
  (:documentation "Returns T if there is more content than was returned by CONTENT-HTML-EXCERPT generic-function."))
