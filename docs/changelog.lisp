(uiop:define-package #:staticl-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:staticl-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "HTTP"))
  (0.4.0 2025-01-11
         "* Function STATICL/SITE:SITE now lists all supported keyword arguments.")
  (0.3.1 2024-10-29
         "* `stop` function was exported.
          * A typo was fixed in the documentation.")
  (0.3.0 2024-05-27
         "* Field `url` was added to `content` objects of type `post` and `page`. It contains a full URL of the page and can be used as `canonical` URL in templates.")
  (0.2.0 2024-05-26
         "* `Excerpt` field was added to the `content` objects such as posts and pages. It can be used in the description HTML tags.
          * Filters `remove-html-tags` and `first-line` were added to the template engine base on Closure Templates.")
  (0.1.1 2024-05-11
         "* Fixed error in `serve` command when xdg-open ultility is not available.
          * Installation using Roswell was updated in the docs.")
  (0.1.0 2024-05-10
         "* Initial version."))
