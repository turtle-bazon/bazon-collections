;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections-tests)

(deftestsuite test-bazon-collections () ())

(addtest
    test-array-list
  (flet ((constructor-function ()
	   (make-instance 'array-list))
	 (element-function (number)
	   number)
	 (de-e (number)
	   number)
	 (constructor-function-entity ()
	   (make-instance 'array-list
			  :hash #'test-entity-hash
			  :test #'test-entity-equal))
	 (element-function-entity (number)
	   (make-instance 'test-entity :number number))
	 (de-e-entity (number-entity)
	   (slot-value number-entity 'number))
	 (oddp-entity (number-entity)
	   (oddp (slot-value number-entity 'number)))
	 (evenp-entity (number-entity)
	   (evenp (slot-value number-entity 'number))))
    (test-collection 'array-list
		     #'constructor-function
		     #'element-function
		     #'de-e
		     #'=
		     #'oddp
		     #'evenp)
    (test-collection 'array-list
		     #'constructor-function-entity
		     #'element-function-entity
		     #'de-e-entity
		     #'test-entity-equal
		     #'oddp-entity
		     #'evenp-entity)))
