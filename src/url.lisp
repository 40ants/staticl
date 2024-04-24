(uiop:define-package #:staticl/url
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:import-from #:quri)
  (:import-from #:alexandria
                #:with-gensyms
                #:make-gensym)
  (:export
   #:with-base-url
   #:object-url
   #:absolute-url-p))
(in-package #:staticl/url)


(defvar *base-url*)


(defgeneric object-url (obj &key full)
  (:documentation "Returns a full object URL.
                   A method should return an relative URL, but if case if FULL argument was given,
                   the full url with schema and domain will be returned.

                   Note a call to this method should happen in a context of the WITH-BASE-URL macro,
                   because it is always return a path from the site's root even if FULL is not given
                   (in this case return only the path without a domain).

                   You may wonder: \"Why does we bother to return a path without a domain?\"
                   It is much easier to service such static site locally for debugging purpose, because
                   you don't have to setup a web server and dns resolver.

                   Actually you will need to use FULL argument only in a rare case when you really need
                   and absolute URL, for example in an RSS feed.")
  (:method :around ((obj t) &key full)
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
                                 *base-url*)))))
      (cond
        (full
         (quri:render-uri
          absolute-url))
        (t
         (quri:uri-path absolute-url))))))


(-> absolute-url-p (string)
    (values boolean &optional))

(defun absolute-url-p (url)
  (let ((parsed (quri:uri url)))
    (when (and (quri:uri-scheme parsed)
               (quri:uri-host parsed))
      (values t))))


(-> assert-absolute-url (string)
    (values string &optional))

(defun assert-absolute-url (url)
  (let ((parsed (quri:uri url)))
    (unless (quri:uri-scheme parsed)
      (error "There is no scheme in ~S."
             url))
    (unless (quri:uri-host parsed)
      (error "There is no host in ~S."
             url))
    url))


(defun call-with-base-url (url thunk)
  (let ((*base-url* (assert-absolute-url url)))
    (funcall thunk)))


(defmacro with-base-url ((url) &body body)
  (with-gensyms (thunk)
    `(flet ((,thunk ()
              ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-base-url ,url #',thunk))))
