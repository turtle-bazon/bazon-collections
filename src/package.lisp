;;; -*- lisp -*-

(defpackage :ru.bazon.bazon-collections
  (:nicknames :bazon-collections)
  (:use :cl)
  (:export
   :collection-impls
   :generic-marks

   :abstract-iterator
   :has-next
   :current
   :next
   
   :abstract-collection
   :size
   :empty-p
   :contains
   :iterator
   :find-object
   :find-all-objects
   :clear
   :add-object
   :add-all-objects
   :erase-object
   :erase-all-objects
   :remove-object
   :remove-all-objects

   :abstract-list
   :index-of
   :get-object-at
   :set-object-at
   :insert-object-before
   :insert-object-after
   :insert-objects-before
   :insert-objects-after
   :remove-object-at

   :array-list
   
   :abstract-linear-list
   :peek-object
   :pull-object
   
   :abstract-queue
   :enqueue-object
   :dequeue-object

   :abstract-stack
   :push-object
   :pop-object

   :abstract-set
   :member-object
   :difference-sets
   :intersection-sets
   :union-sets

   :abstract-map
   :contains-key
   :contains-value
   :get-object
   :put-object
   
   :abstract-tree)
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
