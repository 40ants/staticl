(uiop:define-package #:staticl-ci/ci
  (:use #:cl)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/jobs/run-tests
                #:run-tests)
  (:import-from #:40ants-ci/jobs/docs
                #:build-docs)
  (:import-from #:40ants-ci/workflow
                #:defworkflow))
(in-package #:staticl-ci/ci)


(defworkflow linter
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((40ants-ci/jobs/linter:linter
          :asdf-systems ("staticl"
                         "staticl-docs"
                         "staticl-tests")
          ;; Turned off, because
          ;; linter complains about:
          ;; 
          ;; /home/art/projects/staticl/src/user-package.lisp:
          ;;   Unused imports: staticl/site, staticl/rsync, staticl/plugins/sitemap...
          ;; /home/art/projects/staticl/src/utils.lisp:
          ;;   Missing imports: quicklisp-client (quicklisp-client:quickload)
          ;;
          ;; And currently there is no way to ignore these warnings.
          ;; 
          ;; :check-imports t
          )))

(defworkflow docs
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((build-docs :asdf-system "staticl-docs")))


(defworkflow ci
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((run-tests
          :asdf-system "staticl"
          :lisp ("sbcl-bin"
                 "ccl-bin")
          :coverage t)))
