;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass abstract-iterator ()
  ()
  (:documentation "Common iterator interface."))

(def-r-generic has-next (iterator)
  (:documentation "Tests whether has next record in iterator."))

(def-r-generic current (iterator)
  (:documentation "Access to current object in iterator."))

(def-w-generic next (iterator)
  (:documentation "Iterate to next object."))
