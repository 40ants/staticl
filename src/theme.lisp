(uiop:define-package #:staticl/theme
  (:use #:cl)
  (:import-from #:serapeum
                #:->
                #:fmt)
  (:import-from #:utilities.print-items
                #:print-items-mixin)
  (:import-from #:staticl/utils
                #:find-class-by-name)
  (:import-from #:alexandria
                #:remove-from-plistf)
  (:import-from #:utilities.print-items
                #:print-items)
  (:export #:theme
           #:template-vars
           #:render
           #:list-static
           #:copy-static
           #:theme-path
           #:*themes-path*))
(in-package #:staticl/theme)


(defvar *themes-path* nil
  "Pathname to the directory where additional themes could be found.

   StatiCL will search theme in the following order:

   1. Inside `themes/` directory of the site.
   2. Inside a directory pathname pointed to by *THEMES-PATH* variable.
   3. Inside the `themes/` directory in the StatiCL asdf system.")


(defclass theme (print-items-mixin)
  ((path :initarg :path
         :type pathname
         :reader theme-path)))


(defmethod print-items append ((theme theme))
  (list (list :path "~S" (theme-path theme))))


(defgeneric template-vars (site object &key hash )
  (:documentation "Fills a hash-table given as HASH argument with variables for filling a template.

                   If hash is NIL, then a new hash-table should be allocated with EQUAL :TEST argument.

                   Returned hash-table will be used for rendering a template for an OBJECT."))


(defgeneric render (theme template-name vars stream)
  (:documentation "Renders fills template named TEMPLATE-NAME with given VARS and renders into a given STREAM.

                   - NAME argument is a string.
                   - VARS argument is a hash table with string keys."))


(-> load-theme-from-dir (pathname string)
    (values (or null theme) &optional))

(defun load-theme-from-dir (dir theme-name)
  (let* ((theme-dir
           (merge-pathnames
            (make-pathname :directory (list :relative theme-name))
            (uiop:ensure-directory-pathname dir)))
         (config-filename
           (merge-pathnames
            (make-pathname :name "theme"
                           :type "lisp")
            theme-dir)))
    (when (probe-file config-filename)
      (let* ((initargs (uiop:read-file-form config-filename))
             (class-name (getf initargs :class)))
        (unless class-name
          (error "Theme metadata should contain a :CLASS attribute."))
        (remove-from-plistf initargs :class)
        
        (let* ((class (find-class-by-name class-name
                                          :default-package-template "STATICL/THEMES/~A")))
          (apply #'make-instance class
                 :path theme-dir
                 initargs))))))


(-> load-theme (string &key (:site-root (or null pathname)))
    (values theme &optional))

(defun load-theme (name &key site-root)
  (let ((builtin-themes-dir
          (asdf:system-relative-pathname "staticl"
                                         (make-pathname :directory '(:relative "themes"))))
        (site-themes-dir
          (when site-root
            (merge-pathnames
             (make-pathname :directory '(:relative "themes"))
             (uiop:ensure-directory-pathname site-root)))))
    (or (when site-themes-dir
          (load-theme-from-dir site-themes-dir name))
        (when *themes-path*
          (load-theme-from-dir *themes-path* name))
        (load-theme-from-dir builtin-themes-dir
                             name)
        (error "Theme named ~S not found in ~{~A~#[~; and ~:;, ~]~}"
               name
               (remove-if #'null
                          (list site-themes-dir
                                *themes-path*
                                builtin-themes-dir))))))


(defgeneric list-static (theme)
  (:documentation "Returns a list of static files such as CSS, JS, images.

                   Each list item should be a list of two items where first
                   item is an absolute pathname and second is a pathname relative
                   to the root of the site."))


(defgeneric copy-static (theme stage-dir)
  (:documentation "Copies static files such as CSS, JS, images into the STAGE-DIR.

                   Usually it is enough to define a method for LIST-STATIC generic-function.")
  (:method ((theme theme) (stage-dir pathname))
    (loop with target-dir = (uiop:ensure-directory-pathname stage-dir)
          for (source-filename relative-filename) in (list-static theme)
          for target-filename = (merge-pathnames relative-filename
                                                 target-dir)
          do (ensure-directories-exist target-filename)
             (uiop:copy-file source-filename
                             target-filename))))
