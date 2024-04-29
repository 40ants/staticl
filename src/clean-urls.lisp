(uiop:define-package #:staticl/clean-urls
  (:use #:cl)
  (:import-from #:staticl/site
                #:clean-urls-p
                #:site)
  (:import-from #:str
                #:ends-with-p)
  (:import-from #:serapeum
                #:->)
  (:export #:transform-url
           #:transform-filename))
(in-package #:staticl/clean-urls)


(-> clean-url (string)
    (values string &optional))

(defun clean-url (url)
  (cond
    ((ends-with-p "/index.html" url)
     (subseq url 0 (1+ (- (length url)
                          (length "/index.html")))))
    ((ends-with-p ".html" url)
     (concatenate 'string
                  (subseq url 0 (- (length url)
                                   (length ".html")))
                  "/"))
    (t
     url)))


(-> clean-pathname (pathname)
    (values pathname &optional))

(defun clean-pathname (filename)
  (cond
    ((and (string-equal (pathname-type filename)
                    "html")
          (not (string-equal (pathname-name filename)
                             "index")))
     (merge-pathnames
      (make-pathname :directory (list :relative (pathname-name filename))
                     :name "index"
                     :type "html")
      filename))
    (t
     filename)))


(defgeneric transform-url (site url)
  (:documentation "Converts the URL to the form that should be used on the site.

                   If the site has the clean-urls setting enabled, then the URL like /some/page.html will be converted
                   to /some/page/. If clean-urls is not enabled, the URL will remain unchanged.")
  (:method ((site site) (url string))
    (cond
      ((clean-urls-p site)
       (clean-url url))
      (t
       url))))


(defgeneric transform-filename (site filename)
  (:documentation "Converts the pathname to the form that should be used to write content to the disk.

                   If the site has the clean-urls setting enabled, then the filename like some/page.html will be converted
                   to some/page/index.html. If clean-urls is not enabled, the pathname will remain unchanged.")
  (:method ((site site) (filename pathname))
    (cond
      ((and (clean-urls-p site))
       (clean-pathname filename))
      (t
       filename))))
