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

(defmethod initialize-instance :after ((list array-list) &key)
  (with-slots (initial-capacity elements-array)
      list
    (setf elements-array (make-array initial-capacity))))

(defmethod size ((list array-list))
  (with-slots (size)
      list
    size))

#+nil(defmethod iterator )

(defmethod clear ((list array-list))
  (with-slots (size elements-array)
      list
    (loop for i from 0 below size
	 do (setf (aref elements-array i) nil))
    (setf size 0)))

(defmethod add-object ((list array-list) object)
  (insert-object-before list (slot-value list 'size) object))

(defmethod add-all-objects ((list array-list) objects)
  (with-slots (size elements-array)
      list
    (ensure-array-list-capacity list (+ size (length objects)))
    (loop for object in objects
	 do (progn
	      (setf (aref elements-array size) object)
	      (incf size)))))

(defmethod add-all-objects ((list array-list) (objects abstract-collection))
  (with-slots (size elements-array)
      list
    (ensure-array-list-capacity list (+ size (slot-value objects 'size)))
    (let ((iterator (iterator objects)))
      (loop while (has-next iterator)
	   do (progn
		(setf (aref elements-array size) (next iterator))
		(incf size))))))

#+nil(defmethod erase-object)

#+nil(defmethod erase-all-objects)

#+nil(defmethod remove-object)

#+nil(defmethod remove-all-objects)

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

(defun array-list-insert-object (list place-position object)
  (declare (type array-list list)
	   (type integer place-position))
  (with-slots (size elements-array)
      list
    (ensure-array-list-capacity list (+ size 1))
    (array-shift-right elements-array place-position (- size 1) 1)
    (setf (aref elements-array place-position) object)
    (incf size)))

(defmethod insert-object-before ((list array-list) (index integer) object)
  (array-list-insert-object list index object))

(defmethod insert-object-after ((list array-list) (index integer) object)
  (array-list-insert-object list (+ index 1) object))

(defmethod insert-objects-before ((list array-list) (index integer) objects)
  (with-slots (size elements-array)
      list
    (ensure-array-list-capacity list (+ size (length objects)))
    (array-shift-right elements-array index (- size 1) (length objects))
    (loop for object in objects
       do (progn
	    (setf (aref elements-array index) object)
	    (incf index)))
    (incf size (length object))))

(defmethod insert-objects-before ((list array-list) (index integer) (collection abstract-collection))
  )

(defmethod insert-objects-after ((list array-list) (index integer) objects)
  )

(defmethod insert-objects-after ((list array-list) (index integer) (collection abstract-collection))
  )
