;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass abstract-iterator ()
  ()
  (:documentation "Common iterator interface."))

(def-r-generic it-current (iterator)
  (:documentation "Access to current object in iterator."))

(def-w-generic it-next (iterator)
  (:documentation "Iterate to next object and returns if iterate result."))

(def-w-generic it-prev (iterator)
  (:documentation "Iterate to previous object and returns iterate result."))

(def-w-generic it-before-first (iterator)
  (:documentation "Move cursor before first object."))

(def-w-generic it-after-last (iterator)
  (:documentation "Move cursor after last object."))