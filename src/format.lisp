(uiop:define-package #:staticl/format
  (:use #:cl)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:staticl/site
                #:site)
  (:export #:to-html
           #:extract-assets))
(in-package #:staticl/format)


(defgeneric to-html (text format content-file relative-to-content-file)
  (:method :around ((text t) (format t) (content-file t) (relative-to-content-file t))
    (unless (uiop:absolute-pathname-p content-file)
      (error "CONTENT-FILE argument should be an absolute pathname."))
    (unless (uiop:absolute-pathname-p relative-to-content-file)
      (error "RELATIVE-TO-CONTENT-FILE argument should be an absolute pathname."))
    (call-next-method))
  
  (:method ((text string) (format string) (content-file t) (relative-to-content-file t))
    (to-html text
             (make-keyword (string-upcase format))
             content-file
             relative-to-content-file)))


(defgeneric extract-assets (text format content-file)
  (:documentation "Extracts additional content from the text. For example, if markdown content includes an image,
                   this generic-function should return a list with an object of FILE class. This way an image
                   will be copied to the output directory.")
  (:method ((text t) (format t) (content-file t))
    (values nil))
  
  (:method :around ((text t) (format t) (content-file t))
    (unless (uiop:absolute-pathname-p content-file)
      (error "CONTENT-FILE argument should be an absolute pathname."))
    (call-next-method))
  
  (:method ((text string) (format string) (content-file pathname))
    (extract-assets text
                    (make-keyword (string-upcase format))
                    content-file)))
