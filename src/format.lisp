(uiop:define-package #:staticl/format
  (:use #:cl)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:plump)
  (:import-from #:staticl/utils
                #:make-absolute-url)
  (:import-from #:serapeum
                #:->)
  (:import-from #:lquery
                #:$)
  (:export #:to-html
           #:extract-assets))
(in-package #:staticl/format)


(-> make-urls-absolute (string string)
    (values string &optional))

(defun make-urls-absolute (html base-url)
  (let ((doc (plump:parse html)))
    ($ doc
      "img"
      (map (lambda (item)
             (setf (plump:attribute item "src")
                   (make-absolute-url (plump:attribute item "src")
                                      base-url))
             (values))))
    (plump:serialize doc nil)))


(defgeneric to-html (text format content-file relative-to-content-file &key absolute-urls content-url)
  (:documentation "Should render TEXT in given FORMAT as HTML.

                   If ABSOLUTE-URLS argument is specified, then CONTENT-URL also should be specified. ")
  (:method :around ((text t) (format t) (content-file t) (relative-to-content-file t) &key absolute-urls content-url)
    (unless (uiop:absolute-pathname-p content-file)
      (error "CONTENT-FILE argument should be an absolute pathname."))
    (unless (uiop:absolute-pathname-p relative-to-content-file)
      (error "RELATIVE-TO-CONTENT-FILE argument should be an absolute pathname."))

    ;; Instead of URL rewriting in each to-html method we well care
    ;; about it in this :around method.
    (let* ((html (call-next-method)))
      (cond
        (absolute-urls
         (unless content-url
           (error "When ABSOLUTE-URLS argument is given, CONTENT-URL should be given too."))
         (let ((result (make-urls-absolute html content-url)))
           result))
        (t
         html))))
  
  (:method ((text string) (format string) (content-file t) (relative-to-content-file t) &key absolute-urls content-url)
    (declare (ignore absolute-urls content-url))
    (to-html text
             (make-keyword (string-upcase format))
             content-file
             relative-to-content-file)))


(defgeneric extract-assets (text format content-file)
  (:documentation "Extracts additional content from the text. For example, if markdown content includes an image,
                   this generic-function should return a list with an object of STATICL/CONTENT/FILE:FILE class. This way an image
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
