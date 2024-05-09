(uiop:define-package #:staticl/plugins/disqus
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
                #:fmt
                #:->)
  (:export
   #:disqus
   #:disqus-shortname))
(in-package #:staticl/plugins/disqus)


(defparameter *code-to-inject* "
<div id=\"disqus_thread\"></div>
<script type=\"text/javascript\">
  /* * * CONFIGURATION VARIABLES: EDIT BEFORE PASTING INTO YOUR WEBPAGE * * */
  var disqus_shortname = '~A';
  /* * * DON'T EDIT BELOW THIS LINE * * */
  (function() {
    var dsq = document.createElement('script');
    dsq.type = 'text/javascript';
    dsq.async = true;
    dsq.src = 'http://' + disqus_shortname + '.disqus.com/embed.js';
    dsq.setAttribute('data-timestamp', +new Date());
    (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
      })();
</script>
<noscript>Please enable JavaScript to view the <a href=\"http://disqus.com/?ref_noscript\">comments powered by Disqus.</a></noscript>
<a href=\"http://disqus.com\" class=\"dsq-brlink\">comments powered by <span class=\"logo-disqus\">Disqus</span></a>
")


(defclass disqus (plugin)
  ((shortname :initarg :shortname
              :initform (error "Shortname should be given.")
              :type string
              :reader disqus-shortname)))


(-> disqus (string)
    (values disqus &optional))

(defun disqus (shortname)
  "Enables Disqus on the page.

   To make it work, you have to register your site at Disqus and provide a short site name to the function."
  (make-instance 'disqus
                 :shortname shortname))


(defmethod staticl/pipeline:process-items ((site site) (node disqus) content-items)
  (loop for item in content-items
        when (typep item 'staticl/content/post:post)
        do (add-injection item "after_content"
                          (fmt *code-to-inject*
                               (disqus-shortname node)))))
