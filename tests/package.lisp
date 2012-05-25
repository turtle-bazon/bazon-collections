;;; -*- lisp -*-

(defpackage :ru.bazon.bazon-collections-tests
  (:nicknames :bazon-collections-tests)
  (:use :cl
	:bazon-collections
	:lift
	:iterate)
  (:export
   :run-all-tests)
  (:documentation "Common Lisp Collections framework (test package)"))

(in-package :ru.bazon.bazon-collections-tests)

;;;
;;; run-tests
;;;
(defun run-all-tests ()
  (let* ((test-results (iter (for test-suite in (testsuites))
			     (for test-name = (class-name test-suite))
			     (for should-run-tests = (not (or (equal test-name 'TEST-MIXIN)
							      (equal test-name 'LIFT::PROCESS-TEST-MIXIN))))
			     (for test-suite-results = (when should-run-tests
							 (run-tests :suite test-name
								    :report-pathname nil)))
			     (when should-run-tests
			       (collect
				   (list test-suite-results
					 (testsuite-tests test-name)
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
