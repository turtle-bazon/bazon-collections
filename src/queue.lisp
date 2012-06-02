;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass abstract-queue (abstract-collection)
  ()
  (:documentation "Collection that stores objects prior to processing FIFO."))

