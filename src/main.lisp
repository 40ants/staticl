(uiop:define-package #:staticl/main
  (:use #:cl)
  (:import-from #:staticl)
  (:import-from #:staticl/skeleton)
  (:import-from #:defmain
                #:subcommand
                #:defcommand
                #:defmain))
(in-package #:staticl/main)


(defvar *verbose* nil)


(defmain (main :program-name "staticl") ((verbose "Output more information"
                                                  :flag t)
                                         &subcommand)
  (let ((*verbose* verbose))
    (if verbose
        (log:config :debug)
        (log:config :info))
    
    (defmain:subcommand)))


(defcommand (main generate) ((source-dir "A with site's source files."
                                         :default (namestring
                                                   (uiop:ensure-directory-pathname
                                                    *default-pathname-defaults*)))
                             (output-dir "An output directory to write HTML files to."
                                         :default (namestring
                                                   (uiop:ensure-directory-pathname
                                                    (merge-pathnames "stage")))))
  "Generates HTML static site from the source files."
  (let ((source-dir (uiop:ensure-directory-pathname
                     source-dir))
        (output-dir (uiop:ensure-directory-pathname
                     output-dir)))
    (unless (probe-file (merge-pathnames ".staticlrc"
                                         source-dir))
      (format *standard-output*
              "Config file .staticlrc is missing from the ~A directory.~%"
              (namestring source-dir))
      (uiop:quit 1))
    
    (staticl:generate :root-dir source-dir
                      :stage-dir output-dir)
    (when *verbose*
      (format t "Site was written to: ~A~%"
              (namestring output-dir)))))


(defcommand (main new-site) ((output-dir "An output directory to write site files to."
                                         :default (namestring
                                                   (uiop:ensure-directory-pathname
                                                    *default-pathname-defaults*)))
                             (description "Site's description."
                                          :default "A site description.")
                             title url)
  "Creates a new site skeleton with a few posts."
  (let ((full-output-dir
          (staticl:new-site output-dir
                            title
                            url
                            :description description)))

    (when *verbose*
      (format t "Site's content was written to: ~A~%"
              (namestring full-output-dir)))))


(defcommand (main serve) ((source-dir "A with site's source files."
                                      :default (namestring
                                                (uiop:ensure-directory-pathname
                                                 *default-pathname-defaults*)))
                          (output-dir "An output directory to write HTML files to."
                                      :default (namestring
                                                (uiop:ensure-directory-pathname
                                                 (merge-pathnames "stage"))))
                          (port "A port number to listen on. If not given, then will be choosen automatically.")
                          (interface "A network interface to listen on."
                                     :default "localhost"))
  "Serves site's static from OUTPUT-DIR and rebuilds it if some sources in SOURCE-DIR were changed.

   Pages opened in the browser will be reloaded automatically after each rebuild."
  (staticl:serve :root-dir (uiop:ensure-directory-pathname source-dir)
                 :stage-dir (uiop:ensure-directory-pathname output-dir)
                 :in-thread nil
                 :port (handler-case
                           (when port
                             (parse-integer port))
                         (serious-condition ()
                           (format *standard-output* "Unable to parse port \"~A\"."
                                   port)
                           (uiop:quit 1)))
                 :interface interface))
