;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass abstract-collection ()
  ((test :initarg :test
	 :initform #'equal
	 :documentation "Comparator function for contained objects.")
   (hash :initarg :hash
	 :initform #'sxhash
	 :documentation "Hash function for contained objects."))
  (:documentation
   "Root class for collections.
    Collection is a group of objects."))

(def-r-generic size (collection)
  (:documentation "Returns size of collection."))

(def-r-generic empty-p (collection)
  (:documentation "Checks whether collection is empty."))

(def-r-generic contains (collection object)
  (:documentation "Tests if given object contains in collection."))

(def-r-generic iterator (collection &optional condition)
  (:documentation "Returns iterator for given collection."))

(def-r-generic find-object (collection object)
  (:documentation "Finds given object and returns iterator pointing to it."))

(def-r-generic find-all-objects (collection condition)
  (:documentation "Returns iterator that points to all objects that conforms to given condition."))

(def-w-generic clear (collection)
  (:documentation "Clears given collection."))

(def-w-generic add-object (collection object)
  (:documentation "Destructively adds given object to collection."))

(def-w-generic add-all-objects (collection objects)
  (:documentation "Destructively adds all given objects to collection."))

(def-w-generic erase-object (collection iterator)
  (:documentation "Erase object at iterator's current position."))

(def-w-generic erase-all-objects (collection iterator)
  (:documentation "Sequentally removes all object given by iterator."))

(def-w-generic remove-object (collection object)
  (:documentation "Destructively removes given object from collection"))

(def-w-generic remove-all-objects (collection objects)
  (:documentation "Destructively removes all given objects from collection."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod empty-p ((collection abstract-collection))
  (= 0 (size collection)))
