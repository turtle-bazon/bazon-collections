;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass abstract-queue (abstract-linear-list)
  ()
  (:documentation "Collection that stores objects prior to processing FIFO."))

(def-w-generic enqueue-object (queue object)
  (:documentation "Enqueues given object to queue."))

(def-w-generic dequeue-object (queue)
  (:documentation "Dequeues object from queue."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-object ((queue abstract-queue) object)
  (enqueue-object queue object))

(defmethod pull-object ((queue abstract-queue))
  (dequeue-object queue))
