;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass abstract-set (abstract-collection)
  ()
  (:documentation "Collection that contains no duplicate elements."))

(def-r-generic member-object (set object)
  (:documentation "Tests whether given object is member of set."))

(def-r-generic difference-sets (set1 set2)
  (:documentation "Retirns new set that is difference between set1 and set2."))

(def-r-generic intersection-sets (set1 set2)
  (:documentation "Returns new set that is intersection of set1 and set2."))

(def-r-generic union-sets (set1 set2)
  (:documentation "Returns new set that is union of set1 and set2."))

(defmethod member-object ((set abstract-set) object)
  (contains set object))
