;;; -*- lisp -*-

(defpackage :ru.bazon.bazon-collections
  (:nicknames :bazon-collections)
  (:use :cl)
  (:export
   :collection-impls
   :generic-marks

   :abstract-collection
   :size
   :empty-p
   :contains
   :clear
   :add-object
   :add-all-objects
   :remove-object
   :remove-all-objects
   :remove-with-condition

   :abstract-list
   :index-of
   :get-object-at
   :set-object-at
   :insert-object-after
   :insert-object-before
   :remove-object-at)
  (:documentation "Common Lisp Collections framework"))

(in-package :ru.bazon.bazon-collections)

(defparameter *collection-impls* nil)
(defparameter *generic-marks* (make-hash-table))

(defun collection-impls ()
  (copy-seq *collection-impls*))

(defun generic-marks ()
  (let ((new-ht (make-hash-table)))
    (maphash
     (lambda (key value)
       (setf (gethash key new-ht) value))
     *generic-marks*)
    new-ht))

(defun add-generic-meta (fname context params)
  (setf (gethash fname *generic-marks*)
	(list :context context
	      :params params)))

(defmacro def-collection-impl-class (name &rest rest)
  `(eval-when (:execute :load-toplevel :compile-toplevel)
     (defclass ,name ,@rest)
     (push (quote ,name) *collection-impls*)))

(defmacro def-w-generic (name params &rest rest)
  `(eval-when (:execute :load-toplevel :compile-toplevel)
     (defgeneric ,name ,params ,@rest)
     (add-generic-meta (quote ,name) :write (quote ,params))))

(defmacro def-r-generic (name params &rest rest)
  `(eval-when (:execute :load-toplevel :compile-toplevel)
     (defgeneric ,name ,params ,@rest)
     (add-generic-meta (quote ,name) :read (quote ,params))))
