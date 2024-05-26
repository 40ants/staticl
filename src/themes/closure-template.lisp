(uiop:define-package #:staticl/themes/closure-template
  (:use #:cl)
  (:import-from #:closure-template
                #:define-print-syntax
                #:register-print-handler
                #:compile-template)
  (:import-from #:staticl/utils
                #:transform-keys
                #:do-files)
  (:import-from #:staticl/theme
                #:list-static)
  (:import-from #:str
                #:replace-all)
  (:import-from #:local-time
                #:format-timestring)
  (:import-from #:html2text
                #:html2text))
(in-package #:staticl/themes/closure-template)


(defclass closure-template (staticl/theme:theme)
  ((namespace :initarg :namespace
              :type string
              :reader template-namespace)
   (static-files :reader list-static
                 :type list)
   (date-format :initarg :date-format
                :type list
                :reader date-format)
   (datetime-format :initarg :datetime-format
                    :type list
                    :reader datetime-format))
  (:default-initargs
   :namespace (error ":NAMESPACE argument show be given and correspond to the namespace used in *.tmpl files.")
   :date-format local-time:+iso-8601-date-format+
   :datetime-format (append local-time:+iso-8601-date-format+
                            '(#\Space (:hour 2) #\: (:min 2)))
   :static-files nil))


(defgeneric register-user-filters (theme)
  (:documentation "Registers some variable filters useful inside templates.")
  (:method ((theme closure-template))

    ;; This rule should go first, because it has the same prefix
    ;; as "date" rule:
    (define-print-syntax print-datetime "datetime" (:constant t))
    (define-print-syntax print-date "date" (:constant t))
    (define-print-syntax remove-html-tags "remove-html-tags" (:constant t))
    (define-print-syntax first-line "first-line" (:constant t))
    
    (flet ((format-date (params end value)
             (declare (ignore params end))
             (when value
               (format-timestring nil value
                                  :format (date-format theme))))
           (format-datetime (params end value)
             (declare (ignore params end))
             (when value
               (format-timestring nil value
                                  :format (datetime-format theme))))
           (remove-html-tags (params end value)
             (declare (ignore params end))
             (when value
               (html2text value
                          :tags-to-remove (list :img
                                                :style
                                                :script))))
           (first-line (params end value)
             (declare (ignore params end))
             (when value
               (first
                (str:split #\Newline value
                           :omit-nulls t
                           :limit 2)))))
      (register-print-handler :common-lisp-backend
                              'print-date
                              :function #'format-date)
      (register-print-handler :common-lisp-backend
                              'print-datetime
                              :function #'format-datetime)
      (register-print-handler :common-lisp-backend
                              'remove-html-tags
                              :function #'remove-html-tags)
      (register-print-handler :common-lisp-backend
                              'first-line
                              :function #'first-line))))

(defmethod initialize-instance :after ((obj closure-template) &rest initargs &key path &allow-other-keys)
  (declare (ignore initargs))

  (register-user-filters obj)
  
  (let ((static-files nil)
        (root-path (uiop:ensure-directory-pathname path)))
    
    (do-files (filename root-path)
      (let ((ext (pathname-type filename))
            (exts-to-ignore '("lisp" "fasl")))
        (cond
          ((string-equal ext "tmpl")
           (compile-template :common-lisp-backend filename))
          ((not (member ext exts-to-ignore :test #'string-equal))
           (push (list filename
                       (uiop:enough-pathname filename root-path))
                 static-files)))))

    ;; Here we'll save collected static files to
    ;; be able to copy them into the staging dir later.
    (setf (slot-value obj 'static-files)
          static-files)
    (values)))


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
                                    "raw" (with-output-to-string (string-stream)
                                            (render-helper template template-name vars
                                                           string-stream)))
                    stream))))
