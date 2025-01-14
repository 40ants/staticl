(uiop:define-package #:staticl/plugins/autoreload
  (:use #:cl)
  (:import-from #:staticl/pipeline)
  (:import-from #:staticl/plugin
                #:plugin)
  (:import-from #:staticl/site
                #:site)
  (:import-from #:staticl/injections
                #:add-injection
                #:content-with-injections-mixin))
(in-package #:staticl/plugins/autoreload)


(defparameter *code-to-inject* "
<script>
var eventSource = new EventSource('/events');

eventSource.addEventListener('reload-page', function(event) {
  console.log('Reloading the page');
  location.reload();
});

eventSource.onerror = function(err) {
  console.error('EventSource failed:', err);
};
</script>")


(defclass autoreload (plugin)
  ())


(defun autoreload ()
  (make-instance 'autoreload))


(defmethod staticl/pipeline:process-items ((site site) (node autoreload) content-items)
  (loop for item in content-items
        when (typep item 'content-with-injections-mixin)
        do (add-injection item "head"
                          *code-to-inject*)))
