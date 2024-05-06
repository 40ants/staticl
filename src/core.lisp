(uiop:define-package #:staticl
  (:use #:cl)
  (:nicknames #:staticl/core)
  (:import-from #:staticl/skeleton
                #:new-site)
  (:import-from #:staticl/builder
                #:generate)
  (:import-from #:staticl/server
                #:serve)
  (:export #:generate
           #:new-site
           #:serve))
(in-package #:staticl)


