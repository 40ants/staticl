(uiop:define-package #:staticl/plugins/sitemap
  (:use #:cl)
  (:import-from #:staticl/plugin
                #:plugin)
  (:import-from #:staticl/content
                #:write-content-to-stream
                #:get-target-filename
                #:content
                ;; #:preprocess
                )
  (:import-from #:staticl/site
                #:site)
  (:import-from #:serapeum
                #:soft-list-of)
  (:import-from #:cl-sitemaps/builder
                #:make-url
                #:render-sitemap)
  (:import-from #:staticl/url
                #:object-url))
(in-package #:staticl/plugins/sitemap)


(defclass sitemap (plugin)
  ())


(defun sitemap ()
  (make-instance 'sitemap))


(defclass sitemap-file (content)
  ((contents :initarg :contents
             :type (soft-list-of content)
             :reader sitemap-content)))


;; (defmethod preprocess ((site site) (sitemap sitemap) contents)
;;   (error "Old function will be removed!")
;;   ;; (list (make-instance 'sitemap-file
;;   ;;                      :contents contents))
;;   )


(defmethod get-target-filename ((site site) (sitemap sitemap-file) stage-dir)
  (merge-pathnames (make-pathname :name "sitemap"
                                  :type "xml")
                   stage-dir))


(defmethod write-content-to-stream ((site site) (sitemap sitemap-file) (stream stream))
  (render-sitemap (loop for item in (sitemap-content sitemap)
                        collect (make-url (object-url site item :full t)
                                          :changefreq :weekly
                                          :priority 0.5))
                  :stream stream))


(defmethod staticl/pipeline:process-items ((site site) (node sitemap) content-items)
  (staticl/pipeline:produce-item
   (make-instance 'sitemap-file
                  :contents content-items)))
