(uiop:define-package #:staticl/format/spinneret
  (:use #:cl)
  (:import-from #:staticl/format)
  (:import-from #:spinneret)
  (:import-from #:alexandria
                #:with-gensyms))
(in-package #:staticl/format/spinneret)


(defmethod staticl/format:to-html ((text string) (format (eql :spinneret)))
  (with-gensyms (spinneret-page-package)
    (let* ((*package* (make-package spinneret-page-package
                                    :use '("COMMON-LISP")))
           (sexps
             (with-input-from-string (s text)
               (uiop:slurp-stream-forms s))))
      (eval `(spinneret:with-html-string ()
               ,@sexps)))))
