(uiop:define-package #:staticl/plugin
  (:use #:cl)
  (:import-from #:log)
  (:import-from #:serapeum
                #:fmt
                #:->)
  (:export #:plugin
           #:make-plugin))
(in-package #:staticl/plugin)


(defclass plugin ()
  ())


(-> make-plugin (symbol &rest t)
    (values plugin &optional))

(defun make-plugin (name &rest initargs)
  (let* ((class-name (string-upcase name))
         (package-name (fmt "STATICL/PLUGINS/~A"
                            class-name))
         (class-symbol (uiop:find-symbol* class-name package-name nil))
         (class (when class-symbol
                  (find-class class-symbol))))
    (unless class
      (error "Unable to find plugin class ~A in package ~A."
             class-name
             package-name))
    
    (apply #'make-instance class
           initargs)))
