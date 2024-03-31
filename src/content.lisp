(uiop:define-package #:staticl/content
  (:use #:cl)
  (:import-from #:staticl/theme
                #:template-vars)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:org.shirakumo.fuzzy-dates)
  (:import-from #:staticl/theme
                #:template-vars)
  (:import-from #:staticl/site
                #:site-theme
                #:site-content-root
                #:site)
  (:import-from #:alexandria
                #:with-output-to-file
                #:length=)
  (:import-from #:staticl/utils
                #:normalize-plist
                #:do-files)
  (:import-from #:staticl/content/reader
                #:read-content-file)
  (:import-from #:local-time
                #:universal-to-timestamp
                #:timestamp)
  (:import-from #:serapeum
                #:soft-list-of)
  (:import-from #:utilities.print-items
                #:print-items-mixin
                #:print-items)
  (:export #:supported-content-types
           #:content-type
           #:content
           #:read-contents
           #:read-content-from-disk
           #:content-class
           #:write-content-to-stream
           #:write-content
           #:preprocess
           #:get-target-filename
           #:content-with-title-mixin
           #:content-with-tags-mixin
           #:content-from-file))
(in-package #:staticl/content)


(defclass content-type ()
  ((file-type :initarg :type
              :type string
              :reader content-file-type)
   (content-class :initarg :content-class
                  :reader content-class
                  :type standard-class)))


(defmethod initialize-instance :around ((instance content-type) &rest initargs)
  (when (getf initargs :content-class)
    (setf (getf initargs :content-class)
          (find-class (getf initargs :content-class))))

  (apply #'call-next-method
         instance
         initargs))


(defclass content (print-items-mixin)
  ())


(defclass content-with-title-mixin ()
  ((title :initarg :title
          :type string
          :reader content-title)))


(defmethod print-items append ((obj content-with-title-mixin))
  (list (list :title "~S" (content-title obj))))


(defclass content-with-tags-mixin ()
  ((tags :initarg :tags
         :type (soft-list-of string)
         :reader content-tags)))


(defclass content-from-file (content-with-title-mixin
                             content-with-tags-mixin
                             content)
  ((format :initarg :format
           :type string
           :reader content-format)
   (template :initarg :template
             :type string
             :reader content-template)
   (created-at :initarg :created-at
               :type timestamp
               :reader content-created-at)
   (file :initarg :file
         :type pathname
         :reader content-file
         :documentation "Absolute pathname to the file read from disk or NIL for content objects which have no source file, like RSS feeds.")
   (text :initarg :text
         :type string
         :reader content-text))
  (:default-initargs
   :template (error "Please, specify :TEMPLATE initarg in subclass of CONTENT-FROM-FILE.")))


(defmethod print-items append ((obj content-from-file))
  (list (list :file (list :after :title) "= ~S" (content-file obj))))


;; (defmethod print-object ((obj content) stream)
;;   (print-unreadable-object (obj stream :type t)
;;     (when (and (slot-boundp obj 'title)
;;                (slot-boundp obj 'file))
;;       (format stream "~S :file ~S"
;;               (content-title obj)
;;               (content-file obj)))))


(defmethod initialize-instance ((obj content) &rest initargs)
  (apply #'call-next-method
         obj
         (normalize-plist initargs
                          :created-at (lambda (value)
                                        (etypecase value
                                          (null value)
                                          (string
                                           (universal-to-timestamp
                                            (org.shirakumo.fuzzy-dates:parse value)))
                                          (local-time:timestamp
                                           value)))
                          :tags (lambda (value)
                                  (etypecase value
                                    (string
                                     (mapcar #'str:trim
                                             (str:split "," value
                                                        :omit-nulls t))))))))


(defgeneric supported-content-types (site)
  (:documentation "Returns a list of CONTENT-TYPE objects.")

  (:method :around ((site site))
    (loop with types = (make-hash-table :test 'equal)
          with all-content-types = (call-next-method)
          for content-type in all-content-types
          do (push content-type
                   (gethash (content-file-type content-type)
                            types))
          finally (return
                    (progn
                      (loop for type being the hash-key of types
                              using (hash-value content-types)
                            unless (length= 1 content-types)
                              do (error "There are ~A content-type objects having the same ~S type: ~{~S~^, ~}"
                                        (length content-types)
                                        type
                                        (mapcar #'class-name
                                                (mapcar #'class-of content-types))))
                      ;; Returning original list if there is no duplicates:
                      (values all-content-types))))))


(defgeneric read-content-from-disk (site content-type)
  (:documentation "Returns a list of CONTENT objects corresponding to a given content type")

  (:method ((site site) (content-type content-type))
    (uiop:while-collecting (collect)
      (do-files (file (site-content-root site)
                 :file-type (content-file-type content-type))
        (let* ((args (read-content-file file))
               (obj (apply #'make-instance (content-class content-type)
                           args)))
          (collect obj))))))


(defgeneric read-contents (site)
  (:documentation "Returns a list of CONTENT objects loaded from files.")
  
  (:method ((site site))
    (loop for content-type in (supported-content-types site)
          append (read-content-from-disk site content-type))))


(defgeneric write-content (site content stage-dir)
  (:documentation "Writes CONTENT object to the STAGE-DIR.")
  
  (:method ((site site) (content content) (stage-dir pathname))
    (let* ((target-filename (get-target-filename site content stage-dir)))
      (ensure-directories-exist target-filename)
      
      (with-output-to-file (stream target-filename :if-exists :supersede)
        (write-content-to-stream site content stream))
      (values))))


(defgeneric get-target-filename (site content stage-dir)
  (:documentation "Should return an absolute pathname to a file where this content item should be rendered.")

  (:method ((site site) (content content) (stage-dir pathname))
    (let ((relative-path (enough-namestring (content-file content)
                                            (site-content-root site))))
      (merge-pathnames
       (merge-pathnames (make-pathname :type "html")
                        relative-path)
       stage-dir))))


(defgeneric write-content-to-stream (site content stream)
  (:documentation "Writes CONTENT object to the STREAM using given FORMAT.")

  (:method ((site site) (content content-from-file) (stream stream))
    (let* ((theme (site-theme site))
           (content-vars (template-vars content))
           (site-vars (template-vars site))
           (vars (dict "site" site-vars
                       "content" content-vars))
           (template-name (content-template content)))
      
      (staticl/theme:render theme template-name vars stream))))


(defgeneric preprocess (site plugin content-objects)
  (:documentation "Returns an additional list content objects such as RSS feeds or sitemaps."))


(defmethod template-vars ((content content-from-file) &key (hash (dict)))
  (setf (gethash "title" hash)
        (content-title content)
        (gethash "html" hash)
        (staticl/format:to-html (content-text content)
                                (content-format content)))
  (if (next-method-p)
      (call-next-method content :hash hash)
      (values hash)))
