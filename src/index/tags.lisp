(uiop:define-package #:staticl/index/tags
  (:use #:cl)
  (:import-from #:staticl/pipeline)
  (:import-from #:staticl/index/base
                #:page-target-path
                #:page-size
                #:index-target-path
                #:index-page
                #:base-index)
  (:import-from #:staticl/index/base
                #:base-index)
  (:import-from #:serapeum
                #:->
                #:dict
                #:fmt)
  (:import-from #:alexandria
                #:curry
                #:rcurry)
  (:import-from #:staticl/content
                #:content-with-title-mixin
                #:content-title
                #:content-tags
                #:content-with-tags-mixin)
  (:import-from #:staticl/tag
                #:tag
                #:tag-name)
  (:import-from #:staticl/site
                #:site)
  (:import-from #:staticl/theme
                #:template-vars)
  (:import-from #:staticl/url
                #:object-url)
  (:import-from #:staticl/utils
                #:slot-documentation)
  (:export
   #:tags-index
   #:page-filename-fn
   #:page-title-fn))
(in-package #:staticl/index/tags)


(deftype function-from-string-to-string ()
  '(function (string) (values string &optional)))


(deftype function-from-string-to-pathname ()
  '(function (string) (values pathname &optional)))


(declaim (ftype function-from-string-to-string
                default-page-title-fn))

(defun default-page-title-fn (tag-name)
  (fmt "Posts with tag \"~A\"" tag-name))


(declaim (ftype function-from-string-to-pathname
                default-page-filename-fn))

(defun default-page-filename-fn (tag-name)
  (make-pathname :name tag-name
                 :type "html"))



(defclass tags-index (base-index)
  ((page-filename-fn :initarg :page-filename-fn
                     :type (or null function-from-string-to-pathname)
                     :documentation "A callback to change page titles.

                                     Accepts single argument - a tag name and should return a pathname

                                     By default, for tag \"foo-bar\" it returns foo-bar.html.

                                     If site has \"clean urls\" setting enabled, then additional
                                     transformation to the pathname will be
                                     applied automatically."
                     :reader page-filename-fn)
   (page-title-fn :initarg :page-title-fn
                  :type (or null function-from-string-to-string)
                  :documentation "A callback to change page titles.

                                  Accepts single argument - a tag name and should return a string.

                                  For example, here is how you can translate page title into a russian:

                                  ```lisp
                                  (tags-index :target-path #P\"ru/\"
                                              :page-title-fn (lambda (tag-name)
                                                               (fmt \"Записи с тегом ~A\" tag-name)))
                                  ```
                                 "
                  :reader page-title-fn))
  (:default-initargs
   :page-title-fn #'default-page-title-fn
   :page-filename-fn #'default-page-filename-fn))


(defun tags-index (&rest initargs &key target-path page-size template page-title-fn page-filename-fn)
  (declare (ignore target-path page-size template page-title-fn page-filename-fn))
  (apply #'make-instance 'tags-index
         initargs))


(let ((docs
        (fmt "
Creates additional HTML files with post's excerpts grouped by tag names.

By default `some.html`, `another.html` filenames are used, but this
can be overriden by PAGE-FILENAME-FN argument.

The same way page title may be overriden by providing a function as PAGE-TITLE-FN argument.

**Arguments:**

**PAGE-FILENAME-FN**:

~A

**PAGE-TITLE-FN**:

~A
"
             (slot-documentation 'tags-index 'page-filename-fn)
             (slot-documentation 'tags-index 'page-title-fn))))
  (setf (documentation 'tags-index 'function)
        docs))


(defclass bound-tag (tag)
  ((index-page :initarg :index-page
               :reader tag-index-page))
  (:documentation "A tag bound to the index page. A URL for such tags will lead to the index page."))


(defmethod print-object ((tag bound-tag) stream)
  (print-unreadable-object (tag stream :type t)
    (format stream "\"~A\" ~S"
            (tag-name tag)
            (tag-index-page tag))))


(-> upgrade-tag (string index-page content-with-tags-mixin)
    (values &optional))

(defun upgrade-tag (tag-name index-page content)
  (loop for tag in (content-tags content)
        when (string= (tag-name tag)
                      tag-name)
        do (change-class tag 'bound-tag
                         :index-page index-page))
  (values))


(defmethod template-vars ((site site) (tag bound-tag) &key (hash (dict)))
  (let ((hash (call-next-method site tag :hash hash)))
    (setf (gethash "url" hash)
          (object-url site (tag-index-page tag)))
    (values hash)))


(defclass tags-index-page (index-page)
  ((all-tags :initarg :all-tags
             :initform nil
             :accessor all-tags)))


(defmethod print-object ((page tags-index-page) stream)
  (print-unreadable-object (page stream :type t)
    (format stream "\"~A\""
            (page-target-path page))))


(defmethod template-vars ((site site) (page tags-index-page) &key (hash (dict)))
  (let ((hash (call-next-method site page :hash hash)))
    (setf (gethash "tags" hash)
          (mapcar (curry #'template-vars site)
                  (all-tags page)))
    (values hash)))


(defmethod staticl/pipeline:process-items ((site site) (index tags-index) content-items)
  "Here we are grouping all posts by their tags, generate an index page for each tag and upgrade tag instances to the tags bound to their corresponding index pages.

   Posts on index page are sorted by their titles."
  
  (loop with only-posts = (remove-if-not (lambda (item)
                                           (and (typep item 'content-with-tags-mixin)
                                                (typep item 'content-with-title-mixin)))
                                         content-items)
        with by-tag = (dict)
        for post in only-posts
        do (loop for tag in (content-tags post)
                 do (push post
                          (gethash (tag-name tag) by-tag)))
        finally (loop for tag-name being the hash-key of by-tag
                      using (hash-value tagged-posts)
                      for sorted-posts = (sort tagged-posts #'string<
                                               :key #'content-title)
                      for index-page = (make-instance 'tags-index-page
                                                      :title (funcall (page-title-fn index)
                                                                      tag-name)
                                                      :target-path (merge-pathnames
                                                                    ;; TODO: implement clean urls
                                                                    (funcall (page-filename-fn index)
                                                                             tag-name)
                                                                    (uiop:ensure-directory-pathname
                                                                     (index-target-path index)))
                                                      :items sorted-posts)
                      do (staticl/pipeline:produce-item index-page)
                         ;; Here we change tag classes, to make them
                         ;; render as links to the corresponding index page:
                         (mapc (curry #'upgrade-tag
                                      tag-name
                                      index-page)
                               sorted-posts)
                      collect (make-instance 'bound-tag
                                             :name tag-name
                                             :index-page index-page) into all-tags
                      finally (loop for tag in all-tags
                                    for index-page = (tag-index-page tag)
                                    do (setf (all-tags index-page)
                                             all-tags))))
  (values))
