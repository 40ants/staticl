(uiop:define-package #:staticl/feeds/base
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
                #:timestamp>))
(in-package #:staticl/feeds/base)


(defclass feed ()
  ((target-path :initarg :target-path
                :type pathname
                :reader feed-target-path)
   (length-limit :initarg :length-limit
                 :type integer
                 :reader feed-length-limit)
   (feed-type :initarg :feed-type
              :type symbol
              :reader feed-type))
  (:default-initargs
   :target-path (error "TARGET-PATH is a required argument for a feed object.")
   :feed-type (error "FEED-TYPE is a required argument for a feed object.")
   :length-limit 10))


(defclass feed-file (content)
  ((target-path :initarg :target-path
                :type pathname
                :reader target-path)
   (feed-type :initarg :feed-type
              :type symbol
              :reader feed-type)
   (items :initarg :items
          :type (soft-list-of content)
          :reader content-items)))


(defmethod staticl/content:get-target-filename ((site site) (feed-file feed-file) (stage-dir pathname))
  (merge-pathnames (target-path feed-file)
                   stage-dir))


(defmethod staticl/pipeline:process-items ((site site) (node feed) content-items)
  (let* ((only-posts
           (remove-if-not (lambda (item)
                            (typep item 'post))
                          content-items))
         (sorted-items (sort only-posts
                             #'timestamp>
                             :key #'content-created-at)))

    (staticl/pipeline:produce-item
     (make-instance 'feed-file
                    :target-path (feed-target-path node)
                    :feed-type (feed-type node)
                    :items (take (feed-length-limit node)
                                 sorted-items))))
  (values))

(defmethod staticl/content:write-content-to-stream ((site site) (feed-file feed-file) stream)
  (loop for item in (content-items feed-file)
        for feed-entry = (make-instance 'org.shirakumo.feeder:entry
                                        :id (staticl/url:object-url item :full t)
                                        :link (staticl/url:object-url item :full t)
                                        :title (staticl/content:content-title item)
                                        :summary (staticl/content/html-content:content-html-excerpt item)
                                        :content (staticl/content::content-html item))
        collect feed-entry into entries
        finally
           (let* ((feed (make-instance 'org.shirakumo.feeder:feed
                                       :id (staticl/url:object-url site :full t)
                                       :link (staticl/url:object-url site :full t)
                                       :title (staticl/site:site-title site)
                                       :summary (staticl/site:site-description site)
                                       :content entries))
                  (plump-node (org.shirakumo.feeder:serialize-feed feed
                                                                   (feed-type feed-file))))
             (plump:serialize plump-node stream))))
