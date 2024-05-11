(uiop:define-package #:staticl-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:staticl-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "HTTP"))
  (0.1.1 2024-05-11
         "* Fixed error in `serve` command when xdg-open ultility is not available.
          * Installation using Roswell was updated in the docs.")
  (0.1.0 2024-05-10
         "* Initial version."))
