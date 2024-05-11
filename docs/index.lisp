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
  (:import-from #:staticl-docs/sites
                #:@sites)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:staticl-docs/index)

(in-readtable pythonic-string-syntax)


(eval-always
  (defparameter *ignore-words*
    '("JSON"
      "YAML"
      "OSX"
      "HTTP"
      "HTML"
      "DSL"
      "TODO"
      "StatiCL"
      "Unlicense"
      "REPL"
      "ASDF:PACKAGE-INFERRED-SYSTEM"
      "ASDF"
      "40A"
      "API"
      "JS"
      "PR"
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
  (@rationale section)
  (@features section)
  (@introduction section)
  (@roadmap section)
  (@sites section)
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
ros run --eval '(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)' --quit
ros install 40ants-asdf-system
ros install spinneret
ros install 40ants/staticl
```
""")


(defsection @rationale (:title "Rationale")
  "
Why was another static website generator made when the world is already full of them? There are only three reasons.

The first reason is that at 40Ants we prefer to use Common Lisp in our projects, because it is a significant lever and increases the efficiency of the developer. But the choice of static generators of sites written in Common Lisp is not so great. We know of only one such project – Coleslaw. Actually, we used it, but it turned out that it was not flexible enough.

And here we come to reason number two – Coleslaw was originally conceived as a static blog generator and only later it became possible to add arbitrary pages. So its whole architecture revolves around posts and indexes. We wanted something more. For example, the ability to make static landing pages with arbitrary placement of blocks, and remove blog posts in a separate subsection. And also, we really wanted to run a multilingual blog where posts in different languages could be in different folders. And so on.

Of course, we could look at static site generators in other programming languages, for example Hugo or Nikola. But most of them use simple linear configs or awful YAML files, whereas with Lisp we have more extensive site configuration options due to the fact that all the features of the programming language are available to us. The ability to use DSL (Domain Specific Language) for greater expressiveness of site configuration is the third reason why it was decided to make `StatiCL`.
")


(defsection @features (:title "Features")
  "
* Hackable. `StaticCL` probably the most hackable static site builder in the World due to the nature of Common Lisp programming language and it's interactive development approach.
* Suitable for a site of any configuration, not only for a blog.
* Multiplatform. This project is already tested on Linix and OSX.
* Supports different themes.
* Multiple templating engines might be used.
* Many content formats are supported. Markdown is used by default, but you can easily use reStructured text, Spinneret or add your own.
* Plugins and themse can be hosted along with the content of your site.

See also the STATICL-DOCS/ROADMAP::@ROADMAP section.
")


(defautodoc @api (:system "staticl"
                  :ignore-words *ignore-words*))
