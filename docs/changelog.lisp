(uiop:define-package #:staticl-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:staticl-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "HTML"
                              "HTTP"))
  (0.8.0 2025-10-05
         "* Now STATICL/INDEX/PAGINATED:PAGINATED-INDEX renders an empty index page in case if there is no items.")
  (0.7.1 2025-09-05
         "* Fixed an issue when some pages were not rendered because paginated index used destructive sort operation.")
  (0.7.0 2025-09-04
         "* Custom site variables can now be defined using the VARS argument in site configuration. These variables are available in templates and allow customization of copyright years, hiding/showing elements, and other site-specific data.")
  (0.6.0 2025-08-29
         "* Variable STATICL/THEME:*THEMES-PATH* was added. It can be used to point to a directory with custom themes.")
  (0.5.0 2025-01-29
         "* Now markdown pages can include raw HTML blocks like `<video src=\"images/foo.mp4\">` and these video files will be propertly copied to the output directory during site generation.")
  (0.4.1 2025-01-23
         "* Fixed error when processing a site with filter by path.")
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
