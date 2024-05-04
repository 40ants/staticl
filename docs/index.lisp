(uiop:define-package #:staticl-docs/index
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  #+quicklisp
  (:import-from #:quicklisp)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:staticl-docs/changelog
                #:@changelog)
  (:import-from #:docs-config
                #:docs-config)
  (:import-from #:40ants-doc/autodoc
                #:defautodoc)
  (:import-from #:staticl-docs/introduction
                #:@introduction)
  (:import-from #:serapeum
                #:eval-always)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:staticl-docs/index)

(in-readtable pythonic-string-syntax)


(eval-always
  (defparameter *ignore-words*
    '("JSON"
      "HTTP"
      "HTML"
      "TODO"
      "StatiCL"
      "Unlicense"
      "REPL"
      "ASDF:PACKAGE-INFERRED-SYSTEM"
      "ASDF"
      "40A"
      "API"
      "JS"
      "CSS"
      "UTF-8"
      "RSS"
      "CL"
      "URL"
      "URI"
      "RPC"
      "CDN"
      "XML"
      "SEO"
      "GIT")))


(defmethod docs-config ((system (eql (asdf:find-system "staticl-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (ql:quickload "40ants-doc-theme-40ants")
  #-quicklisp
  (asdf:load-system "40ants-doc-theme-40ants")
  
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))
        :root-sections '(@index
                         @api)))


(defsection @index (:title "staticl - Flexible static site generator."
                    :ignore-words *ignore-words*)
  (staticl system)
  "
[![](https://github-actions.40ants.com/40ants/staticl/matrix.svg?only=ci.run-tests)](https://github.com/40ants/staticl/actions)

![Quicklisp](http://quickdocs.org/badge/staticl.svg)
"
  (@installation section)
  (@introduction section)
  ;; (@usage section)
  ;; (@processing-pipeline section)
  ;; (@api section)
  )


(defsection-copy @readme @index)


(defsection @installation (:title "Installation")
  """
You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :staticl)
```
""")


;; (defsection @usage (:title "Usage")
;;   "
;; `StatiCL` is a static site generator. `StatiCL` as a modular architecture and is suitable for any kind of a site
;; be it a blog or a site with pages not included in the feeds. This project was created to overcome limitations of the Coleslaw.
;; ")


;; (defsection @idea (:title "Idea behind the StatiCL")
;;   "
;; Whereas `Coleslaw` has a monilite architecture, `StatiCL` introduces a modular way of building a pipeline for re
;; ")


;; (defsection @processing-pipeline (:title "Processing Pipeline")
;;   "
;; First of all, a STATICL/SITE:SITE object is created and filled with options from `.staticlrc` file.

;; Then STATICL:STAGE function calls STATICL/CONTENT:READ-CONTENT generic-function which returns a list
;; of STATICL/CONTENT:CONTENT objects. On the next stage, initial list of content objects are passed to a
;; generic-function STATICL/CONTENT:PREPROCESS called with a preprocessor returned by a
;; generic-function STATICL/PLUGINS:SITE-PLUGINS as a first argument. Each preprocessor may
;; return additional STATICL/CONTENT:CONTENT objects such as index pages, RSS or ATOM feeds, sitemaps etc.

;; When all content was preprocessed, a generic-function STATICL/CONTENT:WRITE-CONTENT is called
;; on each STATICL/CONTENT:CONTENT object and a SITE object. Content objects are having a format slot,
;; so internally STATICL/CONTENT:WRITE-CONTENT generic-function creates an object of corresponding format class
;; or takes it from the cache and then calls STATICL/CONTENT:WRITE-CONTENT-TO-STREAM using this format object.
;; ")


(defautodoc @api (:system "staticl"
                  :ignore-words *ignore-words*))
