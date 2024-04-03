(uiop:define-package #:staticl/current-root
  (:use #:cl)
  (:import-from #:serapeum
                #:directory-pathname
                #:->)
  (:import-from #:alexandria
                #:with-gensyms)
  (:export #:with-current-root
           #:current-root))
(in-package #:staticl/current-root)


(declaim (type directory-pathname *current-root*))

(defvar *current-root*)


(-> current-root ()
    (values directory-pathname &optional))

(defun current-root ()
  (unless (boundp '*current-root*)
    (error "Function CURRENT-ROOT should be called inside WITH-CURRENT-ROOT scope."))
  (values *current-root*))


(-> call-with-current-root (directory-pathname function))

(defun call-with-current-root (root thunk)
  (let ((*current-root* (uiop:ensure-absolute-pathname
                         (uiop:ensure-directory-pathname root))))
    (funcall thunk)))


(defmacro with-current-root ((root) &body body)
  (with-gensyms (thunk)
    `(flet ((,thunk ()
              ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-current-root ,root #',thunk))))
