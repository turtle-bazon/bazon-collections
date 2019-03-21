;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections-tests)

(defun start-collection-test (collection-class test-functions &optional init-args)
  (flet ((constructor-function ()
	   (eval `(make-instance (quote ,collection-class) ,@init-args)))
	 (element-function (number)
	   number)
	 (de-e (number)
	   number)
	 (constructor-function-entity ()
	   (eval `(make-instance (quote ,collection-class)
				 :hash #'test-entity-hash
				 :test #'test-entity-equal
				 ,@init-args)))
	 (element-function-entity (number)
	   (make-instance 'test-entity :number number))
	 (de-e-entity (number-entity)
	   (slot-value number-entity 'number)))
    (dolist (test-f test-functions)
      (handler-bind ((condition (lambda (c)
                                  (format t "Condition notified: ~a~&" c))))
	(funcall test-f collection-class
		 #'constructor-function
		 #'element-function
		 #'de-e))
      (handler-bind ((condition (lambda (c)
                                  (format t "Condition notified: ~a~&" c))))
	(funcall test-f collection-class
		 #'constructor-function-entity
		 #'element-function-entity
		 #'de-e-entity)))))

(in-suite all-tests)

(test test-array-list
  (start-collection-test 'array-list
			 (list #'test-collection
			       #'test-list)))

(test test-linked-list
  (start-collection-test 'linked-list
			 (list #'test-collection
			       #'test-list)))

(test test-simple-queue
  (start-collection-test 'simple-queue
			 (list #'test-collection
			       #'test-queue)))

(test test-graham-queue
  (start-collection-test 'graham-queue
			 (list #'test-collection
			       #'test-queue)))

(test test-blocking-queue-back-simple-queue
  (start-collection-test 'blocking-queue
                         (list #'test-blocking-queue)
                         (list :back-queue (make-instance 'simple-queue))))

(test test-blocking-queue-back-graham-queue
  (start-collection-test 'blocking-queue
                         (list #'test-blocking-queue)
                         (list :back-queue (make-instance 'graham-queue))))

(test test-simple-stack
  (start-collection-test 'simple-stack
			 (list #'test-collection
			       #'test-stack)))

(test test-cons-stack
  (start-collection-test 'cons-stack
			 (list #'test-collection
			       #'test-stack)))

(test test-list-set
  (start-collection-test 'cons-set
			 (list #'test-collection
			       #'test-set)))

(test test-hash-set
  (start-collection-test 'hash-set
                         (list #'test-collection
                               #'test-set)))

(test test-hash-map
  (start-collection-test 'hash-map
                         (list #'test-map)))
