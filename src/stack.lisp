;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass abstract-stack (abstract-collection)
  ()
  (:documentation "Collection that stores object prior to processing LIFO."))