(uiop:define-package #:staticl/format/html
  (:use #:cl))
(in-package #:staticl/format/html)


(defmethod staticl/format:to-html ((text string)
                                   (format (eql :html))
                                   (content-file pathname)
                                   (relative-to-content-file pathname)
                                   &key absolute-urls content-url)
  (declare (ignore absolute-urls content-url))
  (values text))
