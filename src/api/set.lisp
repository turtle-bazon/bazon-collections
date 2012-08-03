;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass abstract-set (abstract-collection)
  ()
  (:documentation "Collection that contains no duplicate elements."))

(def-r-generic member-object (set object)
  (:documentation "Tests whether given object is member of set."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod member-object ((set abstract-set) object)
  (contains set object))
