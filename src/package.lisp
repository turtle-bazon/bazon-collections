;;; -*- lisp -*-

(defpackage :ru.bazon.bazon-collections
  (:nicknames :bazon-collections)
  (:use :cl
        :bordeaux-threads)
  (:export
   ; api
   
   :abstract-iterator
   :abstract-decorating-iterator
   :conditional-iterator
   :built-in-list-iterator
   :it-current
   :it-next
   :it-prev
   :it-before-first
   :it-after-last
   
   :abstract-collection
   :size
   :empty-p
   :full-p
   :contains
   :iterator
   :find-object
   :find-all-objects
   :clear
   :add-object
   :add-all-objects
   :remove-object
   :remove-all-objects

   :abstract-list
   :array-index-out-of-bounds
   :in-range-p
   :index-of
   :get-object-at
   :set-object-at
   :insert-object-before
   :insert-object-after
   :insert-all-objects-before
   :insert-all-objects-after
   :remove-object-at
   
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

   :abstract-map
   :contains-key
   :contains-value
   :get-object
   :rem-object
   :put-object
   
   :abstract-tree

   ; implementations
   
   :array-list
   :linked-list

   :graham-queue
   :simple-queue
   :blocking-queue

   :cons-stack
   :simple-stack

   :cons-set
   :hash-set

   :hash-map)
  (:documentation "Common Lisp Collections library"))

(in-package :ru.bazon.bazon-collections)
