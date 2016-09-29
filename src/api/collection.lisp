;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass abstract-collection ()
  ((test
    :initarg :test
    :initform #'equal
    :documentation "Comparator function for contained objects.")
   (hash
    :initarg :hash
    :initform #'sxhash
    :documentation "Hash function for contained objects."))
  (:documentation
   "Root class for collections.
    Collection is a group of objects."))

(defgeneric oequal-p (collection o1 o2)
  (:documentation "Tests for objects equality using :test and :hash functions. Users should initialize collection with it's own equal and hash providers to overwrite equality function."))

(defgeneric size (collection)
  (:documentation "Returns size of collection."))

(defgeneric empty-p (collection)
  (:documentation "Checks whether collection is empty."))

(defgeneric contains (collection object)
  (:documentation "Tests if given object contains in collection."))

(defgeneric iterator (collection &optional condition)
  (:documentation "Returns iterator for given collection."))

(defgeneric find-object (collection object)
  (:documentation "Finds given object and returns iterator pointing to it."))

(defgeneric find-all-objects (collection condition)
  (:documentation "Returns iterator that points to all objects that conforms to given condition."))

(defgeneric clear (collection)
  (:documentation "Clears given collection."))

(defgeneric add-object (collection object)
  (:documentation "Destructively adds given object to collection."))

(defgeneric add-all-objects (collection objects)
  (:documentation "Destructively adds all given objects to collection."))

(defgeneric remove-object (collection object)
  (:documentation "Destructively removes given object from collection"))

(defgeneric remove-all-objects (collection objects)
  (:documentation "Destructively removes all given objects from collection."))

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
  (let ((iterator (make-instance 'built-in-list-iterator :list objects)))
    (add-all-objects collection iterator)))

(defmethod add-all-objects ((collection abstract-collection) (objects abstract-collection))
  (let ((iterator (iterator objects)))
    (add-all-objects collection iterator)))

(defmethod add-all-objects ((collection abstract-collection) (iterator abstract-iterator))
  (loop while (it-next iterator)
	do (add-object collection (it-current iterator))))

(defmethod remove-object ((collection abstract-collection) object)
  (remove-object collection (find-object collection object)))

(defmethod remove-all-objects ((collection abstract-collection) (objects list))
  (remove-all-objects
   collection
   (find-all-objects
    collection
    (lambda (object)
      (member object objects
	      :test (lambda (o1 o2)
		      (oequal-p collection o1 o2)))))))

(defmethod remove-all-objects ((collection abstract-collection) (objects abstract-collection))
  (remove-all-objects
   collection
   (find-all-objects
    collection
    (lambda (object)
      (contains collection object)))))

(defmethod remove-all-objects ((collection abstract-collection) (iterator abstract-iterator))
  (loop while (it-next iterator)
     do (remove-object collection iterator)))

(defmethod print-object ((collection abstract-collection) stream)
  (print-unreadable-object (collection stream :type t :identity t)
    (let((size (size collection)))
      (princ "(" stream)
      (princ (size collection) stream)
      (princ ")" stream)
      (princ " " stream)
      (princ "(" stream)
      (let ((iterator (iterator collection)))
	(when (it-next iterator)
	  (princ (it-current iterator) stream)
	  (loop for i from 0 to 8
		while (it-next iterator)
		do (progn
		     (princ " " stream)
		     (princ (it-current iterator) stream)))))
      (when (> size 10)
	(princ " ..." stream))
      (princ ")" stream))))
