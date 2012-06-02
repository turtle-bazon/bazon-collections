;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass abstract-ordered-list (abstract-list)
  ()
  (:documentation "List that store element in specific order."))

(def-r-generic peek-object (list)
  (:documentation "Retreives, but not removes element from list."))
