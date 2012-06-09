;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass abstract-list (abstract-collection)
  ()
  (:documentation "Collection of ordered elements."))

(def-r-generic sub-list (list start-index end-index)
  (:documentation "Returns new list that sublist of given list (start-index inclusive and end-index exclusive)"))

(def-r-generic index-of (list object)
  (:documentation "Returns index of given object."))

(def-r-generic get-object-at (list index)
  (:documentation "Gets object at given index."))

(def-w-generic set-object-at (list index object)
  (:documentation "Destructively sets object at given index."))

(def-w-generic insert-object-after (list index object)
  (:documentation "Destructively inserts object after object by given index."))

(def-w-generic insert-object-before (list index object)
  (:documentation "Destructively inserts object before object by given index."))

(def-w-generic remove-object-at (list index)
  (:documentation "Destructively removes object by given index."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

