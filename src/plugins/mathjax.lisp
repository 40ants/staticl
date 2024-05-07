(uiop:define-package #:staticl/plugins/mathjax
  (:use #:cl)
  (:import-from #:staticl/plugin
                #:plugin)
  (:import-from #:staticl/site
                #:site)
  (:import-from #:staticl/injections
                #:add-injection
                #:content-with-injections-mixin))
(in-package #:staticl/plugins/mathjax)


(defparameter *code-to-inject* "
<script>
MathJax = {
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
  ())


(defun mathjax ()
  (make-instance 'mathjax))


(defmethod staticl/pipeline:process-items ((site site) (node mathjax) content-items)
  (loop for item in content-items
        when (typep item 'content-with-injections-mixin)
        do (add-injection item "head"
                          *code-to-inject*)))
