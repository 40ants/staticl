(uiop:define-package #:staticl-tests/core
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing))
(in-package #:staticl-tests/core)


(deftest test-example ()
  (ok t "Replace this test with something useful."))
