(uiop:define-package #:staticl-user
  ;; This package does not use all symbols from CL package intentionally:
  (:use)
  (:nicknames #:staticl/user-package)
  (:import-from #:cl
                #:list
                #:t
                #:nil
                #:lambda
                #:let
                #:in-package)
  (:import-from #:serapeum
                #:fmt)
  ;; API imports
  (:import-from #:staticl/plugins/sitemap
                #:sitemap)
  (:import-from #:staticl/site
                #:site)
  (:import-from #:staticl/content
                #:load-content)
  (:import-from #:staticl/feeds/rss
                #:rss)
  (:import-from #:staticl/feeds/atom
                #:atom)
  (:import-from #:staticl/filter
                #:filter)
  (:import-from #:staticl/rsync
                #:rsync)
  (:import-from #:staticl/index/paginated
                #:paginated-index))
