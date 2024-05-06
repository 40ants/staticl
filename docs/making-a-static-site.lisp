(uiop:define-package #:staticl-docs/making-a-static-site
  (:use #:cl)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax))
(in-package #:staticl-docs/making-a-static-site)


(in-readtable pythonic-string-syntax)


(defsection @introduction (:title "Making a Static Site")
  """

"""
  )
