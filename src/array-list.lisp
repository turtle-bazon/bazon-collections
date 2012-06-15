;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass array-list (abstract-list)
  ((size
    :type fixnum
    :initform 0
    :documentation "Size of list.")
   (initial-capacity
    :type fixnum
    :initarg :initial-capacity
    :initform 16
    :documentation "Back array capaciy grow step")
   (elements-array
    :type (simple-vector *)
    :initform nil
    :documentation "Back array"))
  (:documentation "List based on using arrays."))

(deftype al-it-direction () '(member :STEP-FORWARD :STEP-BACKWARD :STEP-UNKNOWN))

(defclass array-list-iterator (abstract-iterator)
  ((array-list :initarg :array-list
	       :type array-list)
   (current-index :initform -1
		  :type fixnum)
   (prev-step :initform :STEP-UNKNOWN
	      :type keyword))
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

(defmethod clear ((list array-list))
  (with-slots (size elements-array)
      list
    (loop for i from 0 below size
	 do (setf (svref elements-array i) nil))
    (setf size 0)))

(defmethod add-object ((list array-list) object)
  (insert-object-before list (slot-value list 'size) object))

(defmethod add-all-objects ((list array-list) objects)
  (insert-objects-before list (slot-value list 'size) objects))

(defmethod remove-object-at-iterator ((list array-list) (iterator array-list-iterator))
  (with-slots (current-index prev-step)
      iterator
    (remove-object-at list current-index)
    (ecase prev-step
      (:STEP-FORWARD (decf current-index))
      (:STEP-BACKWARD (incf current-index)))))

#+nil(defmethod remove-all-objects-in-iterator ((list array-list) (iterator array-list-iterator))
  )

(defmethod get-object-at ((list array-list) (index fixnum))
  (with-slots (elements-array)
      list
    (svref elements-array index)))

(defmethod set-object-at ((list array-list) (index fixnum) object)
  (with-slots (elements-array)
      list
    (setf (svref elements-array index) object)))

(defun ensure-array-list-capacity (list need-size)
  (declare (type array-list list)
	   (type fixnum need-size))
  (with-slots (size elements-array)
      list
    (when (> need-size (length elements-array))
      (let* ((new-array (make-array (floor (* need-size 3/2)))))
	(dotimes (i size)
	  (setf (svref new-array i)
		(svref elements-array i)))
	(setf elements-array new-array)))))

(defun array-shift-right (elements-array from-index to-index count)
  (declare (type (simple-array t (*)) elements-array)
	   (type integer from-index to-index count))
  (loop for i from to-index downto from-index
     do (setf (svref elements-array (+ i count))
	      (svref elements-array i))))

(defmethod insert-object-before ((list array-list) (index fixnum) object)
  (with-slots (size elements-array)
      list
    (when (and (< index 0)
	       (> index size))
      (error 'list-index-out-of-bounds (format nil "op: insert, index: ~a, size: ~a" index size)))
    (ensure-array-list-capacity list (+ size 1))
    (when (< index size)
      (array-shift-right elements-array index (- size 1) 1))
    (setf (svref elements-array index) object)
    (incf size)))

(defmethod insert-objects-before ((list array-list) (index fixnum) objects)
  (with-slots (size elements-array)
      list
    (when (and (< index 0)
	       (> index size))
      (error 'list-index-out-of-bounds (format nil "op: insert, index: ~a, size: ~a" index size)))
    (let ((insert-size (length objects)))
      (ensure-array-list-capacity list (+ size insert-size))
      (when (< index size)
	(array-shift-right elements-array index (- size 1) insert-size))
      (if (typep objects 'abstract-collection)
	  (let ((iterator (iterator (the abstract-collection objects))))
	    (loop
	      for insert-index from index
	      while (it-next iterator)
	      do (setf (svref elements-array insert-index) (it-current iterator))))
	  (loop
	    for object in objects
	    for insert-index from index
	    do (setf (svref elements-array insert-index) object)))      
      (incf size insert-size))))

(defmethod remove-object-at ((list array-list) (index fixnum))
  (with-slots (size elements-array)
      list
    (when (and (< index 0)
	       (>= index size))
      (error 'list-index-out-of-bounds :text (format nil "op: remove, index: ~a, size: ~a" index size)))
    (loop for i from index to (- size 2)
       do (setf (svref elements-array i)
		(svref elements-array (+ i 1))))
    (decf size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod it-current ((iterator array-list-iterator))
  (with-slots (array-list current-index)
      iterator
    (svref (slot-value array-list 'elements-array) current-index)))

(defmethod it-next ((iterator array-list-iterator))
  (with-slots (array-list current-index prev-step)
      iterator
    (incf current-index)
    (setf prev-step :STEP-FORWARD)
    (in-range-p array-list current-index)))

(defmethod it-prev ((iterator array-list-iterator))
  (with-slots (array-list current-index prev-step)
      iterator
    (decf current-index)
    (setf prev-step :STEP-BACKWARD)
    (in-range-p array-list current-index)))

(defmethod it-before-first ((iterator array-list-iterator))
  (with-slots (current-index prev-step)
      iterator
    (setf current-index -1)
    (setf prev-step :STEP-UNKNOWN)))

(defmethod it-after-last ((iterator array-list-iterator))
  (with-slots (array-list current-index prev-step)
      iterator
    (setf current-index (slot-value array-list 'size))
    (setf prev-step :STEP-UNKNOWN)))
