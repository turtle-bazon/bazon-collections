;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass abstract-stack (abstract-ordered-list)
  ()
  (:documentation "Collection that stores object prior to processing LIFO."))

(def-w-generic push-object (stack object)
  (:documentation "Pushes object to stack."))

(def-w-generic pop-object (stack)
  (:documentation "Retreives and removes object from stack."))
