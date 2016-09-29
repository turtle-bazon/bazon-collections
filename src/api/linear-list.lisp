;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass abstract-linear-list (abstract-collection)
  ()
  (:documentation "List that store element in specific order."))

(defgeneric peek-object (list)
  (:documentation "Retreives, but not removes object from list."))

(defgeneric pull-object (list)
  (:documentation "Reterives and removes object from list."))
