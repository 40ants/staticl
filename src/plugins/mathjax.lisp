(uiop:define-package #:staticl/plugins/mathjax
  (:use #:cl)
  (:import-from #:staticl/plugin
                #:plugin)
  (:import-from #:staticl/site
                #:site)
  (:import-from #:staticl/injections
                #:add-injection
                #:content-with-injections-mixin)
  (:import-from #:staticl/content
                #:has-tag-p)
  (:import-from #:serapeum
                #:->))
(in-package #:staticl/plugins/mathjax)


;; Here we explicitly tell MathJax to work only inside "content" and "excerpt"
;; HTML nodes, because "staticl-page" class should be on each "body" node
;; in StatiCL themes.
(defparameter *code-to-inject* "
<script>
MathJax = {
  options: {
    ignoreHtmlClass: 'staticl-page',
    processHtmlClass: 'content|excerpt'
  },
  tex: {
    inlineMath: [['$', '$'], ['\\(', '\\)'], ['\\[', '\\]']]
  },
  svg: {
    fontCache: 'global'
  }
};
</script>

<script src='https://polyfill.io/v3/polyfill.min.js?features=es6'></script>
<script id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>

")


(defclass mathjax (plugin)
  ((force :initarg :force
          :initform nil
          :reader force-mathjax-p)
   (tag-name :initarg :tag-name
             :initform "math"
             :reader math-tag-name)))


(-> mathjax (&key
             (:force boolean)
             (:tag-name (or null string)))
    (values mathjax &optional))

(defun mathjax (&key force (tag-name "math"))
  "Enables MathJAX on the page if it's content has tag equal to the TAG-NAME or if FORCE argument was given."
  (make-instance 'mathjax
                 :force force
                 :tag-name tag-name))


(defmethod staticl/pipeline:process-items ((site site) (node mathjax) content-items)
  (loop for item in content-items
        when (and (typep item 'content-with-injections-mixin)
                  (or (force-mathjax-p node)
                      (has-tag-p item (math-tag-name node))))
        do (add-injection item "after_content"
                          *code-to-inject*)))
