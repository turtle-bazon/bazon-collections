;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections-tests)

(deftestsuite test-bazon-collections () ())

(addtest
    test-array-list
  (let ((constructor-function (lambda ()
				(make-instance 'array-list))))
    (test-collection 'array-list constructor-function)))

#+nil(addtest
    test-simple-pooling
  (let ((threads-before-start (length (bordeaux-threads:all-threads)))
	(result 0)
	(lock (bordeaux-threads:make-lock))
	(thread-pool (make-instance 'thread-pool :name "test" :min-size 1)))
    (start-pool thread-pool)
    (execute thread-pool (thread-function 0 lock result 1 nil))
    (execute thread-pool (thread-function 0 lock result 2 nil))
    (stop-pool thread-pool)
    (join-pool thread-pool)
    (ensure-same 3 result :report "result")
    (ensure-same 0 (slot-value thread-pool 'enhanced-thread-pool::workers-count) :report "workers-size")
    (ensure-same t (enhanced-thread-pool::empty-p (slot-value thread-pool 'enhanced-thread-pool::idle-workers)) :report "pool-empty")
    (ensure-same threads-before-start (length (bordeaux-threads:all-threads))) :report "threads"))

