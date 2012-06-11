;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass array-list (abstract-list)
  ((size
    :type (integer 0 *)
    :initform 0
    :documentation "Size of list.")
   (initial-capacity
    :type (integer 1 *)
    :initarg :initial-capacity
    :initform 10
    :documentation "Back array capaciy grow step")
   (elements-array
    :type (simple-array t (*))
    :initform nil
    :documentation "Back array"))
  (:documentation "List based on using arrays."))

(defclass array-list-iterator (abstract-iterator)
  ((array-list :initarg :array-list
	       :type array-list)
   (current-index :initform -1
		  :type (integer -1 *)))
  (:documentation "Iterator over array list."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((list array-list) &key)
  (with-slots (initial-capacity elements-array)
      list
    (setf elements-array (make-array initial-capacity))))

(defmethod size ((list array-list))
  (slot-value list 'size))

(defmethod iterator ((list array-list) &optional condition)
  (let ((iterator (make-instance 'array-list-iterator :array-list list)))
    (if condition
	"conditional iterator"
	iterator)))

#+nil(defmethod find-object )

#+nil(defmethod find-all-objects)

(defmethod clear ((list array-list))
  (with-slots (size elements-array)
      list
    (loop for i from 0 below size
	 do (setf (aref elements-array i) nil))
    (setf size 0)))

(defmethod add-object ((list array-list) object)
  (insert-object-before list (slot-value list 'size) object))

(defmethod add-all-objects ((list array-list) objects)
  (insert-objects-before list (slot-value list 'size) objects))

#+nil(defmethod remove-object)

#+nil(defmethod remove-all-objects)

#+nil(defmethod remove-object-at-iterator)

#+nil(defmethod remove-all-objects-in-iterator)

#+nil(defmethod index-of)

(defmethod get-object-at ((list array-list) (index integer))
  (with-slots (elements-array)
      list
    (aref elements-array index)))

(defmethod set-object-at ((list array-list) (index integer) object)
  (with-slots (elements-array)
      list
    (setf (aref elements-array index) object)))

(defun ensure-array-list-capacity (list need-size)
  (declare (type array-list list)
	   (type integer need-size))
  (with-slots (size elements-array)
      list
    (when (> need-size (length elements-array))
      (let ((new-array (make-array (* need-size 2))))
	(dotimes (i size)
	  (setf (aref new-array i)
		(aref elements-array i)))
	(setf elements-array new-array)))))

(defun array-shift-right (elements-array from-index to-index count)
  (declare (type (simple-array t (*)) elements-array)
	   (type integer from-index to-index count))
  (loop for i from to-index downto from-index
     do (setf (aref elements-array (+ i count))
	      (aref elements-array i))))

(defmethod insert-object-before ((list array-list) (index integer) object)
  (with-slots (size elements-array)
      list
    (when (and (< index 0)
	       (> index size))
      (error 'list-index-out-of-bounds (format nil "op: insert, index: ~a, size: ~a" index size)))
    (ensure-array-list-capacity list (+ size 1))
    (array-shift-right elements-array index (- size 1) 1)
    (setf (aref elements-array index) object)
    (incf size)))

(defmethod insert-object-after ((list array-list) (index integer) object)
  (insert-object-before list (+ index 1) object))

(defmethod insert-objects-before ((list array-list) (index integer) objects)
  (with-slots (size elements-array)
      list
    (when (and (< index 0)
	       (> index size))
      (error 'list-index-out-of-bounds (format nil "op: insert, index: ~a, size: ~a" index size)))
    (let ((insert-size (length objects)))
      (ensure-array-list-capacity list (+ size insert-size))
      (array-shift-right elements-array index (- size 1) insert-size)
      (if (typep objects 'abstract-collection)
	  (let ((iterator (iterator (the abstract-collection objects))))
	    (loop
	       for insert-index from index
	       while (it-has-next iterator)
	       do (setf (aref elements-array insert-index) (it-next iterator))))
	  (loop
	     for object in objects
	     for insert-index from index
	     do (setf (aref elements-array insert-index) object)))      
      (incf size insert-size))))

(defmethod insert-objects-after ((list array-list) (index integer) objects)
  (insert-object-before list (+ index 1) objects))

(defmethod remove-object-at ((list array-list) (index integer))
  (with-slots (size elements-array)
      list
    (when (and (< index 0)
	       (>= index size))
      (error 'list-index-out-of-bounds :text (format nil "op: remove, index: ~a, size: ~a" index size)))
    (loop for i from index to (- size 2)
       do (setf (aref elements-array i)
		(aref elements-array (+ i 1))))
    (decf size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod it-has-next ((iterator array-list-iterator))
  (with-slots (array-list current-index)
      iterator
    (> (- (slot-value array-list 'size) current-index) 1)))

(defmethod it-has-prev ((iterator array-list-iterator))
  (with-slots (current-index)
      iterator
    (> current-index 0)))

(defmethod it-current ((iterator array-list-iterator))
  (with-slots (array-list current-index)
      iterator
    (aref (slot-value array-list 'elements-array) current-index)))

(defmethod it-next ((iterator array-list-iterator))
  (with-slots (array-list current-index)
      iterator
    (with-slots (size elements-array)
	array-list
      (if (it-has-next iterator)
	  (progn
	    (incf current-index)
	    (aref elements-array current-index))
	  (error 'list-index-out-of-bounds
		 :text (format nil "op: next, index: ~a, size: ~a" (+ current-index 1) size))))))

(defmethod it-prev ((iterator array-list-iterator))
  (with-slots (array-list current-index)
      iterator
    (with-slots (size elements-array)
	array-list
      (if (it-has-prev iterator)
	  (progn
	    (decf current-index)
	    (aref elements-array current-index))
	  (error 'list-index-out-of-bounds
		 :text (format nil "op: prev, index: ~a, size: ~a" (- current-index 1) size))))))
