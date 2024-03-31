(uiop:define-package #:staticl/themes/closure-template
  (:use #:cl)
  (:import-from #:closure-template
                #:compile-template)
  (:import-from #:staticl/utils
                #:transform-keys
                #:do-files)
  (:import-from #:staticl/theme)
  (:import-from #:str
                #:replace-all))
(in-package #:staticl/themes/closure-template)


(defclass closure-template (staticl/theme:theme)
  ((namespace :initarg :namespace
              :type string
              :reader template-namespace))
  (:default-initargs
   :namespace (error ":NAMESPACE argument show be given and correspond to the namespace used in *.tmpl files.")))


(defmethod initialize-instance :after ((obj closure-template) &rest initargs &key path &allow-other-keys)
  (declare (ignore initargs))
  
  (do-files (filename path :file-type "tmpl")
    (compile-template :common-lisp-backend filename)))


(defun normalize-key (text)
  "Transforms dict key to a THIS_UGLY_FORMAT which can be used from Closure Templates."
  (replace-all "-" "_"
               (string-upcase text)))


(defun render-helper (template template-name vars stream)
  (let* ((package-name (string-upcase (template-namespace template)))
         (func-name (string-upcase template-name))
         (symbol (find-symbol func-name package-name))
         (transformed-vars (transform-keys vars
                                           #'normalize-key)))
    (unless (and symbol
                 (fboundp symbol))
      (error "Unable to render template because there is no ~A function in package ~A."
             func-name
             package-name))

    (write-string (funcall symbol transformed-vars)
                  stream)))


(defmethod staticl/theme:render ((template closure-template) (template-name string) (vars hash-table) (stream stream))
  (cond
    ((string-equal template-name "base")
     (render-helper template template-name vars stream))
    (t
     (render-helper template "base"
                    (serapeum:dict* vars
                                    ;; Closure templates do not have
                                    ;; inheritance, so we just render
                                    ;; inner part of the page separately:
                                    :raw (with-output-to-string (string-stream)
                                           (render-helper template template-name vars
                                                          string-stream)))
                    stream))))
