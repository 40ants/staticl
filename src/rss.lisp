(uiop:define-package #:staticl/rss
  (:use #:cl)
  (:import-from #:staticl/site
                #:site)
  (:import-from #:org.shirakumo.feeder)
  (:import-from #:staticl/content
                #:content-file
                #:content-created-at
                #:content)
  (:import-from #:serapeum
                #:take
                #:soft-list-of)
  (:import-from #:staticl/content/post
                #:post)
  (:import-from #:local-time
                #:timestamp>)
  (:export #:rss))
(in-package #:staticl/rss)


(defclass rss ()
  ((target-path :initarg :target-path
                :type pathname
                :reader rss-target-path)
   (length-limit :initarg :length-limit
                 :type integer
                 :reader rss-length-limit))
  (:default-initargs
   :target-path #P"rss.xml"
   :length-limit 10))


(defun rss (&key (target-path #P"rss.xml"))
  (make-instance 'rss
                 :target-path (pathname target-path)))


(defclass rss-file (content)
  ((target-path :initarg :target-path
                :type pathname
                :reader target-path)
   (items :initarg :items
          :type (soft-list-of content)
          :reader content-items)))


(defmethod staticl/content:get-target-filename ((site site) (rss-file rss-file) (stage-dir pathname))
  (merge-pathnames (target-path rss-file)
                   stage-dir))


(defmethod staticl/pipeline:process-items ((site site) (node rss) content-items)
  (let* ((only-posts
           (remove-if-not (lambda (item)
                            (typep item 'post))
                          content-items))
         (sorted-items (sort only-posts
                             #'timestamp>
                             :key #'content-created-at)))

    (staticl/pipeline:produce-item
     (make-instance 'rss-file
                    :target-path (rss-target-path node)
                    :items (take (rss-length-limit node)
                                 sorted-items))))
  (values))


(defmethod staticl/content:write-content-to-stream ((site site) (rss-file rss-file) stream)
  (loop for item in (content-items rss-file)
        for feed-entry = (make-instance 'org.shirakumo.feeder:entry
                                        :id (staticl/url:object-url item)
                                        :link (staticl/url:object-url item)
                                        :title (staticl/content:content-title item)
                                        :summary (staticl/content/html-content:content-html-excerpt item)
                                        :content (staticl/content::content-html item))
        collect feed-entry into entries
        finally
           (let* ((feed (make-instance 'org.shirakumo.feeder:feed
                                       :id (staticl/url:object-url site)
                                       :link (staticl/url:object-url site)
                                       :title (staticl/site:site-title site)
                                       :summary (staticl/site:site-description site)
                                       :content entries))
                  (plump-node (org.shirakumo.feeder:serialize-feed feed 'org.shirakumo.feeder:rss)))
             (plump:serialize plump-node stream))))
