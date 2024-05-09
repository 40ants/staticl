(uiop:define-package #:staticl-docs/roadmap
  (:use #:cl)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax))
(in-package #:staticl-docs/roadmap)


(in-readtable pythonic-string-syntax)

(defsection @roadmap (:title "Roadmap")
  """
* Add support for translations for themes and menu items to be able to make multilingual sites.
* Add a documentation on theme and plugin creation.
* Implement more themes.
* Port all [Coleslaw plugins](https://github.com/coleslaw-org/coleslaw/tree/master/plugins).
* Support more services for adding comments.
* Make cool demos for YouTube and publish them at [40Ants channel](https://www.youtube.com/channel/UCeQ6iZT5nmAGFHX1b4V6dQw).
"""
  )
