(uiop:define-package #:staticl/url
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:import-from #:alexandria
                #:with-gensyms
                #:make-gensym)
  (:export
   #:with-base-url
   #:object-url
   #:absolute-url-p))
(in-package #:staticl/url)


(defvar *base-url*)


(defgeneric object-url (obj)
  (:documentation "Returns a full object URL.
                   A method can return a relative URL, but in this case a call should happen in a context of WITH-BASE-URL macro.")
  (:method :around ((obj t))
    (let ((result (call-next-method)))
      (cond
        ((absolute-url-p result)
         result)
        (t
         (unless (boundp '*base-url*)
           (error "Unable to make an absolute URL from ~A because OBJECT-URL was called outside of WITH-BASE-URL."
                  result))
         (quri:render-uri
          (quri:merge-uris result
                           *base-url*)))))))


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
