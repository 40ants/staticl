(uiop:define-package #:staticl/format/md
  (:use #:cl)
  (:import-from #:staticl/format)
  (:import-from #:3bmd-code-blocks)
  (:import-from #:3bmd)
  (:import-from #:3bmd-grammar
                #:parse-doc)
  (:import-from #:staticl/content/file
                #:file)
  (:import-from #:str
                #:starts-with-p))
(in-package #:staticl/format/md)


(defun imagep (node)
  (and (eql (car node)
            :image)
       (eql (typep (second node)
                   'list)
            (eql (car (second node))
                 :explicit-link))))


(defun image-file (content-file node)
  (let* ((link-node (second node))
         (markdown-link (or (getf (cdr link-node) :source)
                            (error "There is no :SOURCE in node node ~S."
                                   node))))
    (unless (or (starts-with-p "https://" markdown-link)
                (starts-with-p "http://" markdown-link))
      (merge-pathnames markdown-link
                       content-file))))


(defun closest-common-subdirectory (path1 path2)
  (unless (uiop:absolute-pathname-p path1)
    (error "PATH1 should be an absolute pathname, but it is ~S."
           path1))

  (unless (uiop:absolute-pathname-p path2)
    (error "PATH2 should be an absolute pathname, but it is ~S."
           path2))
  
  (let* ((components1 (pathname-directory (cl-fad:canonical-pathname path1)))
         (components2 (pathname-directory (cl-fad:canonical-pathname path2)))
         (common-components (loop for c1 in components1
                                  for c2 in components2
                                  while (and c1
                                             c2
                                             (string= c1
                                                      c2))
                                  collect c1)))
    (if common-components
      (make-pathname :directory common-components)
      (error "Unable to figure out common subdirectory for ~S and ~S"
             path1
             path2))))



(defun make-relative-path (path relative-to-path)
  (unless (uiop:absolute-pathname-p path)
    (error "PATH should be an absolute pathname, but it is ~S."
           path))

  (unless (uiop:absolute-pathname-p relative-to-path)
    (error "RELATIVE-TO-PATH should be an absolute pathname, but it is ~S."
           relative-to-path))
  
  (let* ((common-pathname (closest-common-subdirectory path
                                                       relative-to-path))
         (path-relative-to-base
           (uiop:enough-pathname
            (cl-fad:canonical-pathname path)
            common-pathname))
         (relative-to-path-relative-to-base
           (uiop:enough-pathname
            (cl-fad:canonical-pathname relative-to-path)
            common-pathname))
         (up-dir (make-pathname :directory (list* :relative
                                                  (loop repeat (1- (length (pathname-directory
                                                                            relative-to-path-relative-to-base)))
                                                        collect :up)))))
    (merge-pathnames path-relative-to-base
                     up-dir)))


(defun replace-image-urls (doc content-file relative-to-content-file)
  (labels ((walk (node)
             (typecase node
               (cons
                  (cond
                    ((imagep node)
                     (cond
                       ((image-file content-file node)
                        ;; Return new list like:
                        ;; (:IMAGE (:EXPLICIT-LINK :LABEL NIL :SOURCE "bar.jpg" :TITLE NIL))
                        (let* ((image (copy-list node))
                               (link (copy-list (second image)))
                               ;; This filename is relative to content-file
                               (filename (image-file content-file node))
                               (absolute-filename (merge-pathnames filename
                                                                   content-file))
                               ;; Here we need to get path relative to the page for which
                               ;; content is rendered. For example, if blog post /blog/foo/
                               ;; uses image ../images/bar.jpg, then when this blog post
                               ;; is rendered as part of the index page /, then image SRC
                               ;; should be transformed to blog/images/bar.jpg.
                               (relative-path
                                 (make-relative-path absolute-filename
                                                     relative-to-content-file)))
                          (setf (getf (cdr link) :source)
                                (uiop:unix-namestring relative-path))
                          (setf (second image)
                                link)
                          image))
                       (t
                        node)))
                    ((alexandria:proper-list-p node)
                     (mapcar #'walk node))
                    (t
                     (cons
                      (walk (cdr node))
                      (walk (car node))))))
               (t
                  node))))
    (walk doc)))


(defmethod staticl/format:to-html ((text string) (format (eql :md)) (content-file pathname) (relative-to-content-file pathname) &key absolute-urls content-url)
  ;; TODO: move this binding to some outer scope and make
  ;; it possible to turn on different extensions via SITE's settings.
  (let* ((3bmd-code-blocks:*code-blocks* t)
         (doc (parse-doc text))
         (processed-doc (replace-image-urls doc content-file relative-to-content-file)))
    (with-output-to-string (str)
      (3bmd:print-doc-to-stream processed-doc str))))


(defmethod staticl/format:extract-assets ((text string) (format (eql :md)) (content-file pathname))
  "Searches images inside markdown documents."
  (let ((doc (parse-doc text)))
    ;; Example of the document "Foo ![](bar.jpg)" :
    ;; ((:PLAIN "Foo" " "
    ;;   (:IMAGE (:EXPLICIT-LINK :LABEL NIL :SOURCE "bar.jpg" :TITLE NIL))))
    (uiop:while-collecting (collect)
      (labels ((walk (node)
                 (typecase node
                   (cons
                      (cond
                        ((imagep node)
                         (when (image-file content-file node)
                           (collect
                               (make-instance 'file
                                              :path (image-file content-file node)))))
                        ((alexandria:proper-list-p node)
                         (mapc #'walk node))
                        (t
                         (walk (car node))
                         (walk (cdr node))))))
                 (values)))
        (walk doc)))))
