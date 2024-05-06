(uiop:define-package #:staticl/server
  (:use #:cl)
  (:import-from #:lack.component)
  (:import-from #:lack.app.file)
  (:import-from #:usocket)
  (:import-from #:clack)
  (:import-from #:docs-builder)
  (:import-from #:fs-watcher)
  (:import-from #:trivial-open-browser
                #:open-browser)
  (:import-from #:lack/util/writer-stream
                #:make-writer-stream)
  (:import-from #:serapeum
                #:->
                #:fmt)
  (:import-from #:bordeaux-threads-2
                #:destroy-thread
                #:thread-alive-p
                #:make-thread
                #:lock
                #:condition-variable))
(in-package #:staticl/server)

(defvar *app* nil)
(defvar *server* nil)
(defvar *thread* nil)


(defun port-available-p (port interface)
  (handler-case (let ((socket (usocket:socket-listen interface port :reuse-address t)))
                  (usocket:socket-close socket))
    (usocket:address-in-use-error (e) (declare (ignore e)) nil)))


(defun available-port (interface)
  "Return a port number not in use from 8000 to 60000."
  (loop for port from 8000 upto 60000
        if (port-available-p port interface)
          return port))


(defun serve-docs (root env)
  (let* ((path-info (string-left-trim (list #\/)
                                      (getf env :path-info)))
         (path (if (uiop:directory-pathname-p path-info)
                   (merge-pathnames "index.html" path-info)
                   path-info))
         (full-path (merge-pathnames path root)))
    (if (probe-file full-path)
        (lack.component:call (make-instance 'lack.app.file:lack-app-file
                                            :root root
                                            :file path)
                             env)
        (list 404
              (list :content-type "text/plain")
              (list (format nil "File ~A not found."
                            full-path))))))


(-> make-app (pathname condition-variable lock))

(defun make-app (root update-condition update-condition-lock)
  (flet ((docs-server-app (env)
           (cond
             ((string-equal (getf env :path-info)
                            "/events")
              (lambda (responder)
                (let* ((event-id 0)
                       (remote-side (fmt "~A:~A"
                                         (getf env :remote-addr)
                                         (getf env :remote-port)))
                       (writer (funcall responder '(200 (:content-type "text/event-stream"
                                                         :cache-control "no-cache"
                                                         :x-accel-buffering "no"))))
                       (stream (make-writer-stream writer)))

                  (handler-case
                      (unwind-protect
                           (loop for event-received = (bt2:with-lock-held (update-condition-lock)
                                                        (bt2:condition-wait update-condition update-condition-lock
                                                                            ;; Every 5 seconds we'll send a ping
                                                                            ;; event to ensure the connection is
                                                                            ;; open or will close connection otherwise.
                                                                            :timeout 5))
                                 do (cond
                                      (event-received
                                       (log:debug "Sending event to reload the page to" remote-side)
                                       (write-string (fmt "id: ~A" (incf event-id)) stream)
                                       (terpri stream)
                                       (write-string "event: reload-page" stream)
                                       (terpri stream)
                                       (write-string "data: {}" stream)
                                       (terpri stream)
                                       (terpri stream))
                                      (t
                                       (log:debug "Sending ping event to" remote-side)
                                       (write-string (fmt "id: ~A" (incf event-id)) stream)
                                       (terpri stream)
                                       (write-string "event: ping" stream)
                                       (terpri stream)
                                       (write-string "data: {}" stream)
                                       (terpri stream)
                                       (terpri stream)
                                       ))
                                    (sleep 1))
                        (finish-output stream))
                    ((or
                      sb-int:broken-pipe
                      ;; To handle "Connection reset by peer" error:
                      sb-int:simple-stream-error) ()
                      (log:debug "Closing connection to"
                                 remote-side))))))
             (t
              (serve-docs root env)))))
    #'docs-server-app))


(defun in-subdir-p (root file)
  (let ((root (namestring root))
        (file (namestring file)))
    (and (> (length file)
            (length root))
         (string-equal root
                       (subseq file 0 (length root))))))


(-> serve (&key
           (:root-dir pathname)
           (:stage-dir pathname)
           (:in-thread t)
           (:port (or null integer))
           (:interface string))
    (values &optional))

(defun serve (&key (root-dir *default-pathname-defaults*)
                   (stage-dir (merge-pathnames (make-pathname :directory '(:relative "stage"))
                                               (uiop:ensure-directory-pathname root-dir)))
                   (in-thread t)
                   port
                   (interface "localhost"))
  (let ((root-dir
          ;; Here we ensure both root and stage dirs are absolute and point to the directories
          (merge-pathnames
           (uiop:ensure-directory-pathname root-dir)))
        (stage-dir
          (merge-pathnames
           (uiop:ensure-directory-pathname stage-dir)))
        (dirs-to-watch
          (list root-dir
                ;; We also want to watch if staticl
                ;; directory changes. This will make it
                ;; easier to edit builtin themes.
                (asdf:system-relative-pathname "staticl"
                                               #P""))))
    
    (when *server*
      (log:debug "Stopping an old server")
      (stop))

    (let* ((real-stage-dir (staticl/builder::generate :root-dir root-dir
                                                      :stage-dir stage-dir))
           (port (or port
                     (available-port interface)))
           (update-condition (bordeaux-threads-2:make-condition-variable :name "Static site updated"))
           (update-condition-lock (bordeaux-threads-2:make-lock :name "Static site updated (lock)"))
           (app (make-app real-stage-dir update-condition update-condition-lock))
           (server (progn
                     (log:info "Starting Clack server to serve site from ~A" real-stage-dir)
                     (clack:clackup app
                                    :port port
                                    :address interface)))
           (url (format nil "http://~A:~A/"
                        interface port)))
      (open-browser url)
    
      (labels ((build-site (changed-file)
                 (unless (in-subdir-p real-stage-dir changed-file)
                   (log:info "File ~A was changed. Rebuilding the site at ~A"
                             changed-file
                             root-dir)
                   (handler-case
                       (staticl/builder::generate :root-dir root-dir
                                                  :stage-dir stage-dir)
                     (serious-condition (condition)
                       (log:error "Unable to build static for ~A system: ~A"
                                  root-dir
                                  condition)))
                   
                   ;; Notifying the browser that it must reload the page:
                   (bordeaux-threads-2:condition-broadcast update-condition)))
               
               (run-site-autobuilder ()
                 (fs-watcher:watch dirs-to-watch #'build-site)))
        (cond
          (in-thread
           (setf *app* app)
           (setf *server* server)
           (setf *thread*
                 (make-thread #'run-site-autobuilder
                              :name (format nil "Site Autobuilder for ~A: ~A"
                                            root-dir url))))
          (t
           (unwind-protect
                (run-site-autobuilder)
             (clack:stop server))))

        (values)))))


(-> stop ()
    (values &optional))

(defun stop ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil))
  (when *thread*
    (when (thread-alive-p *thread*)
      (destroy-thread *thread*))
    (setf *thread*
          nil))

  (values))
