(uiop:define-package #:staticl/feeds/atom
  (:use #:cl)
  (:import-from #:org.shirakumo.feeder)
  (:import-from #:staticl/feeds/base
                #:feed)
  (:shadow #:atom)
  (:export #:atom))
(in-package #:staticl/feeds/atom)


(defclass atom (feed)
  ()
  (:default-initargs
   :feed-type 'org.shirakumo.feeder:atom))


(defun atom (&key (target-path #P"atom.xml"))
  (make-instance 'atom
                 :target-path (pathname target-path)))
