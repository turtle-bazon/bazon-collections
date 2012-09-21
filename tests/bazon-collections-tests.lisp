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
				  (print-backtrace c))))
	(funcall test-f collection-class
		 #'constructor-function
		 #'element-function
		 #'de-e))
      (handler-bind ((condition (lambda (c)
				  (print-backtrace c))))
	(funcall test-f collection-class
		 #'constructor-function-entity
		 #'element-function-entity
		 #'de-e-entity)))))

(deftestsuite test-bazon-collections () ())

(addtest
    test-array-list
  (start-collection-test 'array-list
			 (list #'test-collection
			       #'test-list)))

(addtest
    test-linked-list
  (start-collection-test 'linked-list
			 (list #'test-collection
			       #'test-list)))

(addtest
    test-simple-queue
  (start-collection-test 'simple-queue
			 (list #'test-collection
			       #'test-queue)))

(addtest
    test-graham-queue
  (start-collection-test 'graham-queue
			 (list #'test-collection
			       #'test-queue)))

(addtest
    test-simple-stack
  (start-collection-test 'simple-stack
			 (list #'test-collection
			       #'test-stack)))

(addtest
    test-cons-stack
  (start-collection-test 'cons-stack
			 (list #'test-collection
			       #'test-stack)))

(addtest
    test-list-set
  (start-collection-test 'list-set
			 (list #'test-collection
			       #'test-set)))
