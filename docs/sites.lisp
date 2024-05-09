(uiop:define-package #:staticl-docs/sites
  (:use #:cl)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax))
(in-package #:staticl-docs/sites)


(in-readtable pythonic-string-syntax)

(defsection @sites (:title "Sites Built With StatiCL")
  """
Here is a list of sites which use `StatiCL` to generate HTML:

* https://40ants.com/ ([sources](https://github.com/40ants/40ants.github.com)).

Feel free to create PR to add your site here. It will be intresting to share configurations, themes and plugins!
""")
