(uiop:define-package #:staticl-user
  ;; This package does not use all symbols from CL package intentionally:
  (:use #:cl)
  (:nicknames #:staticl/user-package)
  
  (:import-from #:serapeum
                #:fmt)
  ;; API imports
  (:import-from #:staticl/navigation
                #:menu
                #:item)
  (:import-from #:staticl/links/prev-next
                #:prev-next-links)
  (:import-from #:staticl/plugins/sitemap
                #:sitemap)
  (:import-from #:staticl/plugins/mathjax
                #:mathjax)
  (:import-from #:staticl/site
                #:site)
  (:import-from #:staticl/content-pipeline
                #:load-content)
  (:import-from #:staticl/feeds/rss
                #:rss)
  (:shadowing-import-from #:staticl/feeds/atom
                          #:atom)
  (:import-from #:staticl/filter
                #:filter)
  (:import-from #:staticl/rsync
                #:rsync)
  (:import-from #:staticl/index/paginated
                #:paginated-index)
  (:import-from #:staticl/index/tags
                #:tags-index))
