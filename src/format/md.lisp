(uiop:define-package #:staticl/format/md
  (:use #:cl)
  (:import-from #:staticl/format)
  (:import-from #:3bmd-code-blocks)
  (:import-from #:3bmd))
(in-package #:staticl/format/md)


(defmethod staticl/format:to-html ((text string) (format (eql :md)))
  ;; TODO: move this binding to some outer scope and make
  ;; it possible to turn on different extensions via SITE's settings.
  (let ((3bmd-code-blocks:*code-blocks* t))
    (with-output-to-string (str)
      (3bmd:parse-string-and-print-to-stream text str))))
