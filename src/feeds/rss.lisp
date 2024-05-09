(uiop:define-package #:staticl/feeds/rss
  (:use #:cl)
  (:import-from #:org.shirakumo.feeder)
  (:import-from #:staticl/feeds/base
                #:feed)
  (:export #:rss))
(in-package #:staticl/feeds/rss)


(defclass rss (feed)
  ()
  (:default-initargs
   :feed-type 'org.shirakumo.feeder:rss))


(defun rss (&key (target-path #P"rss.xml")
                 (limit 10))
  "Creates an XML feed in Rss format at TARGET-PATH.

   Only a LIMIT latest posts are included into the feed."
  (make-instance 'rss
                 :target-path (pathname target-path)
                 :length-limit limit))
