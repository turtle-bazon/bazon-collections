;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass collection ()
  ()
  (:documentation
   "Root class for collections.
    Collection is a group of objects."))

(defgeneric size (collection)
  (:documentation "Returns size of collection."))

(defgeneric empty-p (collection)
  (:documentation "Checks whether collection is empty."))
