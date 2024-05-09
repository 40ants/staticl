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
  (:import-from #:40ants-doc/locatives/asdf-system
                #:asdf-system-documentation-title)
  (:import-from #:staticl-docs/making-a-static-site
                #:@making-a-site)
  (:import-from #:staticl-docs/pipeline
                #:@pipeline)
  (:import-from #:staticl-docs/roadmap
                #:@roadmap)
  (:import-from #:staticl-docs/contribution
                #:@contribution)
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


(defmethod asdf-system-documentation-title ((system (eql (asdf:find-system "staticl"))))
  "ASDF System Details")


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
                         @making-a-site
                         @pipeline
                         @api)))


(defsection @index (:title "StatiCL - Flexible static site generator"
                    :ignore-words *ignore-words*)
  ;; ""
  
  (staticl system)
  "
<table>
<tr>
<td>
<a href=\"https://github.com/40ants/staticl/actions\"><img src=\"https://github-actions.40ants.com/40ants/staticl/matrix.svg?only=ci.run-tests\"/></a>
</td>
<td rowspan=2>
<img src=\"https://storage.yandexcloud.net/40ants-public/staticl/small-logo.webp\" title=\"StatiCL Logo\"/>
</td>
</tr>
<tr>
<td>
<img src=\"http://quickdocs.org/badge/staticl.svg\" title=\"Quicklisp\"/>
</td>
</tr>
</table>
"
  
  (@installation section)
  (@introduction section)
  (@roadmap section)
  (@contribution section))


(defsection-copy @readme @index)


(defsection @installation (:title "Installation")
  """
This library is not in Quicklisp yet, but you can install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :staticl)
```

Or, if you wish to use a command line utility, then install it with [Roswell](https://github.com/roswell/roswell) like this:

```
ros install 40ants/staticl
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
