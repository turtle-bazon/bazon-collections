;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass abstract-list (abstract-collection)
  ()
  (:documentation "Collection of ordered elements."))

(def-r-generic in-range-p (list index)
  (:documentation "Test whether given index in list range."))

(def-r-generic index-of (list object)
  (:documentation "Returns index of given object."))

(def-r-generic get-object-at (list index)
  (:documentation "Gets object at given index."))

(def-w-generic set-object-at (list index object)
  (:documentation "Destructively sets object at given index."))

(def-w-generic insert-object-before (list index object)
  (:documentation "Destructively inserts object before object by given index."))

(def-w-generic insert-object-after (list index object)
  (:documentation "Destructively inserts object after object by given index."))

(def-w-generic insert-all-objects-before (list index object)
  (:documentation "Destructively inserts objects before object by given index."))

(def-w-generic insert-all-objects-after (list index object)
  (:documentation "Destructively inserts objects after object by given index."))

(def-w-generic remove-object-at (list index)
  (:documentation "Destructively removes object by given index."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition list-index-out-of-bounds (error)
  ((text :initarg :text
	 :reader :text))
  (:documentation "Error of index out of bounds."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-object ((list abstract-list) object)
  (insert-object-before list (slot-value list 'size) object))

(defmethod add-all-objects ((list abstract-list) objects)
  (insert-all-objects-before list (slot-value list 'size) objects))

(defmethod remove-object ((list abstract-list) (iterator abstract-iterator))
  (remove-object-at list iterator))

(defmethod in-range-p ((list abstract-list) (index fixnum))
  (with-slots (size)
      list
    (and (>= index 0)
	 (< index size))))

(defmethod index-of ((list abstract-list) object)
  (let ((iterator (iterator list)))
    (loop for index from 0
	  while (it-next iterator)
	  when (oequal-p list object (it-current iterator))
	    return index)))
