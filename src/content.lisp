(uiop:define-package #:staticl/content
  (:use #:cl)
  (:import-from #:staticl/theme
                #:template-vars)
  (:import-from #:serapeum
                #:->
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
                #:+iso-8601-date-format+
                #:format-timestring
                #:universal-to-timestamp
                #:timestamp)
  (:import-from #:serapeum
                #:soft-list-of)
  (:import-from #:utilities.print-items
                #:print-items-mixin
                #:print-items)
  (:import-from #:closer-mop
                #:slot-definition-name
                #:class-slots
                #:slot-definition-initargs)
  (:import-from #:staticl/tag
                #:tag)
  (:import-from #:staticl/format
                #:to-html)
  (:import-from #:staticl/url
                #:object-url)
  (:import-from #:staticl/current-root
                #:current-root)
  (:import-from #:staticl/content/html-content
                #:content-html-excerpt
                #:content-html)
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
           #:content-from-file
           #:content-created-at
           #:content-format
           #:content-template
           #:content-file
           #:content-text
           #:content-title
           #:content-excerpt-separator
           #:set-metadata
           #:load-content))
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
  ((metadata :initform (make-hash-table :test 'equal)
             :type hash-table
             :reader content-metadata
             :documentation "A hash with additional fields specified in the file's header.")))


(defclass content-with-title-mixin ()
  ((title :initarg :title
          :type string
          :reader content-title)))


(defmethod print-items append ((obj content-with-title-mixin))
  (list (list :title "~S" (content-title obj))))


(defclass content-with-tags-mixin ()
  ((tags :initarg :tags
         :type (soft-list-of tag)
         :reader content-tags))
  (:default-initargs
   :tags nil))


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
   (url :initarg :url
        :type (or null string)
        :documentation "Page's URL or a path relative to Site's URL.")
   (text :initarg :text
         :type string
         :reader content-text)
   (excerpt-separator :initarg :excerpt
                      :type string
                      :reader content-excerpt-separator))
  (:default-initargs
   :template (error "Please, specify :TEMPLATE initarg in subclass of CONTENT-FROM-FILE.")
   :url nil
   :excerpt "<!--more-->"))


(defmethod print-items append ((obj content-from-file))
  `(((:file (:after :title)) " file = ~S" ,(content-file obj))))


(defmethod initialize-instance ((obj content) &rest initargs &key &allow-other-keys)
  (let* ((normalized-args
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
                                      (list value)
                                      (string
                                       (loop for tag-name in (str:split "," value
                                                                        :omit-nulls t)
                                             collect (make-instance 'tag
                                                                    :name (str:trim tag-name))))))))
         (result (apply #'call-next-method obj normalized-args))
         (all-initargs
           (loop for slot in (class-slots (class-of obj))
                 appending (slot-definition-initargs slot))))

    ;; Write unknown initargs into the metadata slot
    (loop for (key value) on initargs by #'cddr
          unless (member key all-initargs)
            do (setf (gethash (string-downcase key)
                              (content-metadata result))
                     value))
    
    (values result)))


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


(defmethod object-url ((content content-from-file) &key &allow-other-keys)
  (or (slot-value content 'url)
      (let* ((root (current-root))
             (relative-path (enough-namestring (content-file content)
                                               root)))
        (uiop:unix-namestring
         (merge-pathnames (make-pathname :type "html")
                          relative-path)))))


(defgeneric write-content-to-stream (site content stream)
  (:documentation "Writes CONTENT object to the STREAM using given FORMAT.")

  (:method ((site site) (content content) (stream stream))
    (let* ((theme (site-theme site))
           (content-vars (template-vars content))
           (site-vars (template-vars site))
           (vars (dict "site" site-vars
                       "content" content-vars))
           (template-name (content-template content)))

      (staticl/theme:render theme template-name vars stream))))


(defgeneric preprocess (site plugin content-objects)
  (:documentation "Returns an additional list content objects such as RSS feeds or sitemaps."))


(defmethod template-vars :around ((content content) &key (hash (dict)))
  (loop with result = (if (next-method-p)
                          (call-next-method content :hash hash)
                          (values hash))
        for key being the hash-key of (content-metadata content)
          using (hash-value value)
        do (setf (gethash key result)
                 (typecase value
                   ;; For local-time timestamp we want to leave as is
                   ;; because it's formatting may depend on a template.
                   (local-time:timestamp
                    value)
                   ;; Here we need transform CLOS objects to hash-tables
                   ;; to make their fields accessable in the template
                   (standard-object
                    (template-vars value))
                   ;; Other types are passed as is:
                   (t
                    value)))
        finally (return result)))


(defmethod content-html ((content content-from-file))
  (to-html (content-text content)
           (content-format content)))



(defmethod content-html-excerpt ((content content-from-file))
  (let* ((separator (content-excerpt-separator content))
         (full-content (content-text content))
         (excerpt (first
                   (str:split separator
                              full-content
                              :limit 2))))
    (to-html excerpt
             (content-format content))))


(defmethod template-vars ((content content-from-file) &key (hash (dict)))
  (setf (gethash "title" hash)
        (content-title content)
        (gethash "html" hash)
        (content-html content)
        (gethash "created-at" hash)
        (content-created-at content)
        
        ;; (gethash "created-at" hash)
        ;; (format-timestring nil (content-created-at content)
        ;;                    :format local-time:+iso-8601-format+)
        ;; ;; Also we provide only a date, because it might be more convinitent
        ;; ;; to render it in short format:
        ;; (gethash "created-at-date" hash)
        ;; (format-timestring nil (content-created-at content)
        ;;                    :format +iso-8601-date-format+)

        ;; ;; And in case if user wants a complied to ISO format:
        ;; (gethash "created-at-iso" hash)
        ;; (format-timestring nil (content-created-at content)
        ;;                    :format local-time:+iso-8601-format+)
        )
  
  (if (next-method-p)
      (call-next-method content :hash hash)
      (values hash)))


(defmethod template-vars ((content content-with-tags-mixin) &key (hash (dict)))
  (setf (gethash "tags" hash)
        (mapcar #'template-vars
                (content-tags content)))
  
  (if (next-method-p)
      (call-next-method content :hash hash)
      (values hash)))


(-> set-metadata (content string t &key (:override-slot boolean))
    (values t &optional))

(defun set-metadata (content key value &key override-slot)
  "Changes metadata dictionary by adding a new item with key KEY.

   Key should be a string and it is automatically downcased.

   Note, this way, you can override content's object slots.
   To prevent accidential override, function will raise an error
   in case if a slot named KEY exists in the object CONTENT.
   To force override provide OVERRIDE-SLOT argument."
  
  (unless override-slot
    (when (member key
                  (loop for slot in (class-slots (class-of content))
                        collect (slot-definition-name slot))
                  :test #'string-equal)
      (cerror "Ignore and override"
              "Key ~S already exists as a slot of ~S class.
To override this slot provide :OVERRIDE-SLOT T argument to SET-METADATA function."
              key
              (class-name (class-of content)))))
  
  (setf (gethash (string-downcase key)
                 (content-metadata content))
        value))
