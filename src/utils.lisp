(uiop:define-package #:staticl/utils
  (:use #:cl)
  (:import-from #:log)
  (:import-from #:quri)
  (:import-from #:str
                #:trim-left)
  (:import-from #:serapeum
                #:maphash-new
                #:fmt
                #:->)
  (:import-from #:alexandria
                #:proper-list
                #:with-gensyms)
  (:import-from #:cl-fad
                #:walk-directory)
  (:export
   #:do-files
   #:normalize-plist
   #:absolute-url-p
   #:assert-absolute-url))
(in-package #:staticl/utils)


(-> load-system (string)
    (values &optional))

(defun load-system (name)
  (log:info "Loading system" name)
  #+quicklisp
  (ql:quickload name)
  #-quicklisp
  (asdf:load-system name)
  (values))


(-> split-package-and-symbol-name (string &key (:default-package-template (or null string)))
    (values string
            string
            &optional))


(defun split-package-and-symbol-name (text &key default-package-template)
  (cond
    ((find #\: text :test #'char=)
     (destructuring-bind (package-name symbol-name)
         (str:split #\: text :omit-nulls t :limit 2)
       (values package-name
               ;; In case if text is "SOME::FOO"
               ;; we need to trim : prefix from symbol-name
               (trim-left symbol-name :char-bag '(#\:)))))
    (t
     (unless default-package-template
       (error "Unable to determine a package name of ~S."
              text))
     
     (values (fmt default-package-template
                  text)
             text))))


(-> find-class-by-name (string &key (:default-package-template (or null string)))
    (values standard-class
            &optional))


(defun find-class-by-name (name &key default-package-template)
  (log:debug "Searching class by name" name)
  (multiple-value-bind (package-name symbol-name)
      (split-package-and-symbol-name name
                                     :default-package-template default-package-template)
    (let ((symbol (uiop:find-symbol* (string-upcase symbol-name)
                                     (string-upcase package-name))))
      (find-class symbol))))


(defmacro do-files ((filename root-path &key file-type) &body body)
  "For each file under ROOT-PATH, run BODY. If FILE-TYPE is provided, only run
BODY on files that match the given extension."
  (with-gensyms (correct-file-type-p thunk)
    `(flet ((,correct-file-type-p (file)
              (string-equal (pathname-type file)
                            ,file-type))
            (,thunk (,filename)
              ,@body))
       (declare (dynamic-extent #',correct-file-type-p
                                #',thunk))
       (walk-directory (uiop:ensure-directory-pathname ,root-path)
                       #',thunk
                       :follow-symlinks nil
                       :test (if ,file-type
                                 #',correct-file-type-p
                                 (constantly t))))))


(-> normalize-plist (proper-list &key &allow-other-keys)
    (values proper-list &optional))

(defun normalize-plist (plist &rest normalizers-plist &key &allow-other-keys)
  "Returns a new list where each value is replaced with results of call of normalizing functions.

   For example:

   ```
   CL-USER> (normalize-plist '(:foo \"Bar\" :blah 123)
                             :foo (lambda (value)
                                    (alexandria:make-keyword (string-upcase value))))
   (LIST :FOO :BAR :BLAH 123)
   ```
"
  (loop for (key value) on plist by #'cddr
        for normalizer = (getf normalizers-plist key)
        append (list key
                     (if normalizer
                         (funcall normalizer value)
                         value))))


(-> load-files-from (pathname)
    (values &optional))

(defun load-files-from (path)
  (let ((path (uiop:ensure-directory-pathname path)))
    (when (probe-file path)
      (flet ((lisp-file-p (filename)
               (string-equal (pathname-type filename)
                             "lisp")))
        (declare (dynamic-extent #'lisp-file-p))
       
        (cl-fad:walk-directory path
                               #'load
                               :test #'lisp-file-p))))
  (values))



(-> transform-keys (hash-table (function (string)
                                         (values string &optional)))
    (values hash-table &optional))

(defun transform-keys (dict fn)
  "Transforms to uppercase or string keys of dict and keys of all nested dicts."
  (labels ((rec (obj)
             (typecase obj
               (null
                obj)
               (hash-table
                (maphash-new #'to-upper obj))
               (list
                (mapcar #'rec obj))
               (t obj)))
           (to-upper (key value)
             (values (typecase key
                       (string (funcall fn key))
                       (t key))
                     (rec value))))
    (declare (dynamic-extent #'rec
                             #'to-upper))
    (rec dict)))


(-> absolute-url-p (string)
    (values boolean &optional))

(defun absolute-url-p (url)
  (let ((parsed (quri:uri url)))
    (when (and (quri:uri-scheme parsed)
               (quri:uri-host parsed))
      (values t))))


(-> assert-absolute-url (string)
    (values string &optional))

(defun assert-absolute-url (url)
  (let ((parsed (quri:uri url)))
    (unless (quri:uri-scheme parsed)
      (error "There is no scheme in ~S."
             url))
    (unless (quri:uri-host parsed)
      (error "There is no host in ~S."
             url))
    url))
