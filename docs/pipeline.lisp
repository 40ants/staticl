(uiop:define-package #:staticl-docs/pipeline
  (:use #:cl)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  (:shadowing-import-from #:staticl/user-package
                          #:atom)
  (:import-from #:staticl/user-package
                #:tags-index
                #:paginated-index
                #:filter
                #:atom
                #:mathjax
                #:rss
                #:load-content
                #:prev-next-links
                #:disqus
                #:sitemap))
(in-package #:staticl-docs/pipeline)


(in-readtable pythonic-string-syntax)

(defsection @pipeline (:title "Pipeline"
                              :ignore-words ("HTML"
                                             "XML"
                                             "MathJAX"
                                             "STATICL-USER"))
  (@idea section)
  (@building-blocks section))


(defsection @idea (:title "Idea")
  """
Pipeline is the basis of Static. All the content of the site passes through the pipeline, and is converted at the output to HTML, XML, and other formats. The pipeline principle is used here – each part of the pipeline processes or creates new content elements, and those, in turn, are processed by subsequent pipeline blocks.

In the static site config, the pipeline is assembled using function calls. But these functions do not create any content at the time of loading the configuration file. They only return a description of the pipeline.

For example, the pipeline of the simplest site will consist of only one element: load-content. He is responsible for reading pages and posts from files.

If we want to do something with posts and pages, for example, create for them `sitemap.xml `, then we can add a call to another SITEMAP function after the LOAD-CONTENT function. Both of these functions return the "nodes" of the pipeline. Each node is an object that will then be used when calling generic-function STATICL/PIPELINE:PROCESS-ITEMS to process the content.
""")


(defsection @building-blocks (:title "Building Blocks")
  "All symbols, listed below, are available in the STATICL-USER package and can be used in the `.staticlrc` file without a package prefix."

  "# Main Building Blocks"
  (load-content function)
  (filter macro)

  "# Content Organization"
  (prev-next-links function)
  (paginated-index function)
  (tags-index function)

  "# Feeds"
  
  (atom function)
  (rss function)
  (sitemap function)

  "# Plugins"
  (disqus function)
  (mathjax function))
