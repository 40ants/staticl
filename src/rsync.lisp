(uiop:define-package #:staticl/rsync
  (:use #:cl)
  (:export #:rsync))
(in-package #:staticl/rsync)


(defclass rsync ()
  ((host :initarg :host
         :type string
         :reader rsync-host))
  (:default-initargs
   :host (error ":HOST is required argument for RSYNC node.")))


(defun rsync (host)
  (make-instance 'rsync
                 :host host))
