(uiop:define-package #:staticl-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:staticl-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "HTTP"))
  (0.2.0 2024-05-26
         "* `Excerpt` field was added to the `content` objects such as posts and pages. It can be used in the description HTML tags.
          * Filters `remove-html-tags` and `first-line` were added to the template engine base on Closure Templates.")
  (0.1.1 2024-05-11
         "* Fixed error in `serve` command when xdg-open ultility is not available.
          * Installation using Roswell was updated in the docs.")
  (0.1.0 2024-05-10
         "* Initial version."))
