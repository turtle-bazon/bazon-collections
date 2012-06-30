;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass abstract-stack (abstract-linear-list)
  ()
  (:documentation "Collection that stores object prior to processing LIFO."))

(def-w-generic push-object (stack object)
  (:documentation "Pushes object to stack."))

(def-w-generic pop-object (stack)
  (:documentation "Retreives and removes object from stack."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-object ((stack abstract-stack) object)
  (push-object stack object))

(defmethod pull-object ((stack abstract-stack))
  (pop-object stack))
