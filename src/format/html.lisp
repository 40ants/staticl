(uiop:define-package #:staticl/format/html
  (:use #:cl))
(in-package #:staticl/format/html)


(defmethod staticl/format:to-html ((text string) (format (eql :html)))
  text)
