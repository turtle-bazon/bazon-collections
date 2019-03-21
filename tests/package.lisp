;;; -*- lisp -*-

(defpackage :ru.bazon.bazon-collections-tests
  (:nicknames :bazon-collections-tests)
  (:use
   :cl
   :bazon-collections
   :iterate
   :fiveam)
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

(defun ensure-same (expected result &optional &key report)
  (if report
      (is (eq expected result) report)
      (is (eq expected result))))

(defun ensure-null (result &optional &key report)
  (if report
      (is (eq nil result) report)
      (is (eq nil result))))

(defun ensure (result &optional &key report)
  (if report
      (is-true result report)
      (is-true result)))

(def-suite all-tests
  :description "All tests")
