(uiop:define-package #:staticl/url
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:import-from #:quri)
  (:import-from #:alexandria
                #:with-gensyms
                #:make-gensym)
  (:import-from #:staticl/site
                #:site)
  (:import-from #:staticl/utils
                #:assert-absolute-url
                #:absolute-url-p)
  (:import-from #:staticl/clean-urls
                #:transform-url)
  (:export
   #:with-base-url
   #:object-url))
(in-package #:staticl/url)


(defvar *base-url*)


(defgeneric object-url (site obj &key full clean-urls)
  (:documentation "Returns a full object URL.
                   A method should return an relative URL, but in case if FULL argument was given,
                   the full url with schema and domain will be returned.

                   Note a call to this method should happen in a context of the WITH-BASE-URL macro,
                   because it is always return a path from the site's root even if FULL is not given
                   (in this case return only the path without a domain).

                   You may wonder: \"Why does we bother to return a path without a domain?\"
                   It is much easier to service such static site locally for debugging purpose, because
                   you don't have to setup a web server and dns resolver.

                   Actually you will need to use FULL argument only in a rare case when you really need
                   and absolute URL, for example in an RSS feed.

                   It CLEAN-URLS argument is T, then additional transformation will be applied
                   to the url.")
  (:method :around ((site site) (obj t) &key full (clean-urls t))
    (let* ((result (call-next-method))
           (absolute-url
             (cond
               ((absolute-url-p result)
                (quri:uri result))
               (t
                (unless (boundp '*base-url*)
                  (error "Unable to make an absolute URL from ~A because OBJECT-URL was called outside of WITH-BASE-URL."
                         result))
                (quri:merge-uris result
                                 *base-url*))))
           (url (cond
                  (full
                   (quri:render-uri
                    absolute-url))
                  (t
                   (quri:uri-path absolute-url)))))
      (cond
        (clean-urls
         (transform-url site url))
        (t
         url)))))


(defun call-with-base-url (url thunk)
  (let ((*base-url* (assert-absolute-url url)))
    (funcall thunk)))


(defmacro with-base-url ((url) &body body)
  (with-gensyms (thunk)
    `(flet ((,thunk ()
              ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-base-url ,url #',thunk))))
