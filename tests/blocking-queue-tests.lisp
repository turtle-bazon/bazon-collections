;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections-tests)

(defun test-blocking-queue-dequeue (queue-class constructor-function element-function de-e)
  (let ((queue (funcall constructor-function)))
    (ensure (empty-p queue))
    (enqueue-object queue (funcall element-function 1))
    (ensure-null (empty-p queue))
    (enqueue-object queue (funcall element-function 2))
    (ensure-null (empty-p queue))
    (ensure-same 1 (funcall de-e (dequeue-object queue))
                 :report (report-name queue-class "1 is first dequeue after enqueue 1, 2"))
    (ensure-same 2 (funcall de-e (dequeue-object queue))
                 :report (report-name queue-class "2 is second dequeue after enqueue 1, 2"))
    (ensure (empty-p queue))
    #+sbcl
    (flet ((current-mcs () (multiple-value-bind (secs msecs)
                              (sb-ext:get-time-of-day)
                            (+ (* secs 1000000) msecs))))
      (let ((before-time (current-mcs)))
        (bt:make-thread
         (lambda ()
           (sleep 0.1)
           (enqueue-object queue (funcall element-function 3))))
        (ensure-same 3 (funcall de-e (dequeue-object queue))
                     :report (report-name queue-class "3 is thread enqueue"))
        (let ((diff-time (- (current-mcs) before-time)))
          (ensure (>= diff-time 100000)))
        (ensure (empty-p queue))))
    (clear queue)))

(defun test-blocking-queue (queue-class
                            constructor-function element-function de-e)
  (test-blocking-queue-dequeue queue-class constructor-function element-function de-e))
