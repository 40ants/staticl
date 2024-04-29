(uiop:define-package #:staticl/site-url
  (:use #:cl)
  (:import-from #:staticl/site
                #:site-url
                #:site)
  (:import-from #:staticl/url
                #:object-url))
(in-package #:staticl/site-url)


(defmethod object-url ((site site) (obj site) &key &allow-other-keys)
  (site-url obj))
