(uiop:define-package #:staticl/content/reader
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:scan-to-strings)
  (:import-from #:alexandria
                #:proper-list
                #:make-keyword)
  (:import-from #:serapeum
                #:soft-list-of
                #:->)
  (:export #:read-content-file))
(in-package #:staticl/content/reader)


(defparameter *default-metadata-separator*
  ";;;;;")


(-> parse-initarg (string)
    (values proper-list &optional))

(defun parse-initarg (line)
  "Given a metadata header, LINE, parse an initarg name/value pair from it."
  (let ((name (string-upcase (subseq line 0 (position #\: line))))
        (match (nth-value 1 (scan-to-strings "[a-zA-Z]+:\\s+(.*)" line))))
    (when match
      (list (make-keyword name)
            (aref match 0)))))


(-> parse-metadata (stream &key (:separator string))
    (values proper-list &optional))

(defun parse-metadata (stream &key (separator *default-metadata-separator*))
  "Given a STREAM, parse metadata from it or signal an appropriate condition."
  (flet ((get-next-line (input)
           (string-trim '(#\Space #\Return #\Newline #\Tab) (read-line input nil))))
    (unless (string= (get-next-line stream) separator)
      (error "The file, ~a, lacks the expected header: ~a" (file-namestring stream) separator))
    (loop for line = (get-next-line stream)
          until (string= line separator)
          appending (parse-initarg line))))


(-> read-content (pathname &key (:separator string))
    (values proper-list &optional))

(defun read-content-file (file &key (separator *default-metadata-separator*))
  "Returns a plist of metadata from FILE with :TEXT holding the content going after the SEPARATOR."
  (unless (uiop:absolute-pathname-p file)
    (error "Path ~S should be an absolute pathname."
           file))
  
  (flet ((slurp-remainder (stream)
           (let ((seq (make-string (- (file-length stream)
                                      (file-position stream)))))
             (read-sequence seq stream)
             (remove #\Nul seq))))
    (declare (dynamic-extent #'slurp-remainder))
    
    (with-open-file (in file :external-format :utf-8)
      (let ((metadata (parse-metadata in :separator separator))
            (content (slurp-remainder in))
            ;; (filepath (enough-namestring file (repo-dir *config*)))
            )
        (list* :text content :file file
               metadata)))))
