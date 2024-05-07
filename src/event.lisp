(uiop:define-package #:staticl/event
  (:use #:cl)
  (:import-from #:serapeum
                #:->
                #:fmt)
  (:import-from #:bordeaux-threads-2
                #:destroy-thread
                #:thread-alive-p
                #:make-thread
                #:lock
                #:condition-variable)
  (:import-from #:alexandria
                #:with-gensyms
                #:once-only))
(in-package #:staticl/event)


(defclass event ()
  ((name :initarg :name
         :initform "Event"
         :type string
         :reader %event-name)
   (counter :initform 0
            :accessor %event-counter)
   (condition :initarg :condition
              :type bt2:condition-variable
              :reader %event-condition)
   (lock :initarg :lock
         :type bt2:lock
         :reader %event-lock)))


(defmethod print-object ((event event) stream)
  (print-unreadable-object (event stream :type t)
    (format stream "\"~A\" counter: ~A"
            (%event-name event)
            (%event-counter event))))

(defun make-event (name)
  (make-instance 'event
                 :condition (bt2:make-condition-variable :name (fmt "Event condition \"~A\"" name))
                 :lock (bt2:make-lock :name (fmt "Event lock \"~A\"" name))))


(-> notify (event)
    (values &optional))

(defun notify (event)
  (bt2:with-lock-held ((%event-lock event))
    (incf (%event-counter event))
    (bt2:condition-broadcast (%event-condition event))))


(defvar *event-to-wait*)
(defvar *there-were-new-events-p*)


(defun there-were-new-events-p ()
  (unless (boundp '*there-were-new-events-p*)
    (error "Function THERE-WERE-NEW-EVENTS-P should be called in context of WITH-EVENT-WAITING macro."))
  (funcall *there-were-new-events-p*))


(-> call-with-event-waiting (function event))

(defun call-with-event-waiting (thunk event)
  (when (boundp '*event-to-wait*)
    (error "Unable to nest WITH-EVENT-WAITING forms!"))
  
  (let ((*event-to-wait* event)
        (seen-counter (%event-counter event)))
    (flet ((%there-were-new-events-p ()
             (let ((current-counter (%event-counter event)))
               (prog1 (/= current-counter
                          seen-counter)
                 (setf seen-counter
                       current-counter)))))
      (declare (dynamic-extent #'%there-were-new-events-p))
      
      (let ((*there-were-new-events-p* #'%there-were-new-events-p))
        (funcall thunk)))))


(defmacro with-event-waiting ((event) &body body)
  `(flet ((event-waiting-thunk ()
            ,@body))
     (call-with-event-waiting #'event-waiting-thunk ,event)))


(-> wait (&key (:timeout (or null alexandria:positive-integer)))
    (values t &optional))

(defun wait (&key timeout)
  (unless (and (boundp '*there-were-new-events-p*)
               (boundp '*event-to-wait*))
    (error "Function WAIT should be called in context of WITH-EVENT-WAITING macro."))
  
  (flet ((now ()
           (ceiling (/ (get-internal-real-time)
                       internal-time-units-per-second))))
    (bt2:with-lock-held ((%event-lock *event-to-wait*))
      (loop with wait-until = (when timeout
                                (+ (now)
                                   timeout))
            do (bt2:condition-wait (%event-condition *event-to-wait*)
                                   (%event-lock *event-to-wait*)
                                   :timeout (when wait-until
                                              (max 0
                                                   (- wait-until (now)))))
               (cond
                 ((there-were-new-events-p)
                  (return-from wait t))
                 ((and wait-until
                       (>= (now) wait-until))
                  (return-from wait nil)))))))



;; (defun make-event-waiter (event waiter-id)
;;   (flet ((do-job ()
;;            (loop for result = (wait event :timeout 5)
;;                  do (if result
;;                         (log:info "~A: Event occured" waiter-id)
;;                         (log:info "~A: Timeout" waiter-id)))))
;;     (bt2:make-thread #'do-job
;;                      :name (fmt "Event waiter ~A"
;;                                 waiter-id))))
