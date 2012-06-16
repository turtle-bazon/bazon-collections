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

(def-r-generic oequal-p (collection o1 o2)
  (:documentation "Tests for objects equality using :test and :hash functions. Users should initialize collection with it's own equal and hash providers to overwrite equality function."))

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

(def-w-generic remove-object (collection object)
  (:documentation "Destructively removes given object from collection"))

(def-w-generic remove-all-objects (collection objects)
  (:documentation "Destructively removes all given objects from collection."))

(def-w-generic remove-object-at-iterator (collection iterator)
  (:documentation "Remove object at iterator's current position."))

(def-w-generic remove-all-objects-in-iterator (collection iterator)
  (:documentation "Sequentally removes all object given by iterator."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod oequal-p ((collection abstract-collection) o1 o2)
  (with-slots (test hash)
      collection
    (and o1 o2
	 (= (funcall hash o1)
	    (funcall hash o2))
	 (funcall test o1 o2))))

(defmethod empty-p ((collection abstract-collection))
  (= 0 (size collection)))

(defmethod contains ((collection abstract-collection) object)
  (not (null (find-object collection object))))

(defmethod find-object ((collection abstract-collection) object)
  (let ((iterator (iterator collection)))
    (loop while (it-next iterator)
	  when (oequal-p collection object (it-current iterator))
	    return iterator)))

(defmethod find-all-objects ((collection abstract-collection) condition)
  (iterator collection condition))

(defmethod add-all-objects ((collection abstract-collection) (objects list))
  (loop for object in objects
       do (add-object collection object)))

(defmethod add-all-objects ((collection abstract-collection) (objects abstract-collection))
  (let ((iterator (iterator objects)))
    (loop while (it-next iterator)
	 do (add-object collection (it-current iterator)))))

(defmethod remove-object ((collection abstract-collection) object)
  (remove-object-at-iterator collection (find-object collection object)))

(defmethod remove-all-objects ((collection abstract-collection) (objects list))
  (remove-all-objects-in-iterator
   collection
   (find-all-objects
    collection
    (lambda (object)
      (member object objects
	      :test (lambda (o1 o2)
		      (oequal-p collection o1 o2)))))))

(defmethod remove-all-objects ((collection abstract-collection) (objects abstract-collection))
  (remove-all-objects-in-iterator
   collection
   (find-all-objects
    collection
    (lambda (object)
      (contains collection object)))))

(defmethod remove-all-objects-in-iterator ((collection abstract-collection) (iterator abstract-iterator))
  (loop while (it-next iterator)
     do (remove-object-at-iterator collection iterator)))
