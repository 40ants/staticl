(uiop:define-package #:staticl-user
  ;; This package does not use CL package intentionally
  (:import-from #:cl
                #:list)
  (:import-from #:staticl/plugins/sitemap
                #:sitemap)
  (:import-from #:staticl/site
                #:site)
  (:import-from #:staticl/content
                #:load-content)
  (:import-from #:staticl/rss
                #:rss)
  (:import-from #:staticl/filter
                #:filter)
  (:import-from #:staticl/rsync
                #:rsync)
  (:nicknames #:staticl/user-package))
(in-package #:staticl-user)
