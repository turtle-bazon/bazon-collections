;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass abstract-collection ()
  ()
  (:documentation
   "Root class for collections.
    Collection is a group of objects."))

(def-r-generic size (collection)
  (:documentation "Returns size of collection."))

(def-r-generic empty-p (collection)
  (:documentation "Checks whether collection is empty."))

(def-r-generic contains (collection object)
  (:documentation "Tests if given object contains in collection."))

(def-w-generic clear (collection)
  (:documentation "Clears given collection."))

(def-w-generic add-object (collection object)
  (:documentation "Destructively adds given object to collection."))

(def-w-generic remove-object (collection object &optional &key test)
  (:documentation "Destructively removes given object from collection"))
