(uiop:define-package #:staticl/format
  (:use #:cl)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:staticl/site
                #:site)
  (:export #:to-html))
(in-package #:staticl/format)


(defgeneric to-html (text format)
  (:method ((text string) (format string))
    (to-html text
             (make-keyword (string-upcase format)))))
