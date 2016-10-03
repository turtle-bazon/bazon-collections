;;; -*- lisp -*-

(defpackage :ru.bazon.bazon-collections-tests
  (:nicknames :bazon-collections-tests)
  (:use :cl
	:bazon-collections
	:lift
	:iterate
	:trivial-backtrace)
  (:export
   :run-all-tests)
  (:documentation "Common Lisp Collections library (test package)"))

(in-package :ru.bazon.bazon-collections-tests)

(defclass test-entity ()
  ((number
    :initarg :number
    :type integer
    :initform (error "Define number!!!")))
  (:documentation "Entity for test hash and test functions."))

(defun test-entity-hash (test-entity)
  (declare (type test-entity test-entity))
  (with-slots (number)
      test-entity
    (sxhash number)))

(defun test-entity-equal (te1 te2)
  (declare (type test-entity te1 te2))
  (= (slot-value te1 'number)
     (slot-value te2 'number)))

(defun c-it (iterator)
  (make-instance 'conditional-iterator
		 :condition (lambda (o)
			      (declare (ignore o))
			      t)
		 :back-iterator iterator))

(defun report-name (collection-class name)
  (format nil "~a: ~a" collection-class name))

;;;
;;; run-tests
;;;
(defun run-all-tests ()
  (let* ((test-results (iter (for test-suite in (testsuites))
			     (for should-run-tests = (not (or (equal test-suite 'TEST-MIXIN)
							      (equal test-suite 'LIFT::PROCESS-TEST-MIXIN))))
			     (for test-suite-results = (when should-run-tests
							 (run-tests :suite test-suite
								    :report-pathname nil)))
			     (when should-run-tests
			       (collect
				   (list test-suite-results
					 (testsuite-tests test-suite)
					 (errors test-suite-results)
					 (failures test-suite-results)))))))
    (format t "~%")
    (when test-results
      (destructuring-bind (tests-run tests-error tests-failed)
	  (reduce
	   #'(lambda (x y)
	       (list (+ (first x) (first y))
		     (+ (second x) (second y))
		     (+ (third x) (third y))))
	   (iter (for (_ tests-run-local tests-error-local tests-failed-local) in test-results)
		 (collect (list (length tests-run-local)
				(length tests-error-local)
				(length tests-failed-local)))))
	(iter (for (test-result) in test-results)
	      (describe-test-result test-result t))
	(format t "~%Test Report for all tests: ~a All, ~a!"
		tests-run
		(if (or (> tests-failed 0) (> tests-error 0))
		    (format nil "~a Errors, ~a Failure" tests-error tests-failed)
		    "all passed"))))))
