;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(deftype al-it-direction () '(member :STEP-FORWARD :STEP-BACKWARD :STEP-UNKNOWN))

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
    :type (or (simple-vector *) null)
    :initform nil
    :documentation "Back array"))
  (:documentation "List based on using arrays."))

(defclass array-list-iterator (abstract-iterator)
  ((array-list
    :initarg :array-list
    :type array-list
    :initform (error "Specify array-list."))
   (current-index
    :initform -1
    :type fixnum)
   (prev-step
    :initform :STEP-UNKNOWN
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
        (make-instance 'conditional-iterator :back-iterator iterator
		       :condition condition)
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
  (insert-all-objects-before list (slot-value list 'size) objects))

(defmethod get-object-at ((list array-list) (index fixnum))
  (with-slots (elements-array)
      list
    (svref elements-array index)))

(defmethod set-object-at ((list array-list) (index abstract-iterator) object)
  (let ((iterator (it-native-iterator index)))
    (with-slots (current-index)
	iterator
      (set-object-at list current-index object))))

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

(defmethod insert-object-before ((list array-list) (index abstract-iterator) object)
  (let ((iterator (it-native-iterator index)))
    (with-slots (current-index)
	iterator
      (insert-object-before list current-index object)
      (incf current-index))))

(defmethod insert-object-before ((list array-list) (index fixnum) object)
  (with-slots (size elements-array)
      list
    (when (and (< index 0)
	       (> index size))
      (error 'list-index-out-of-bounds :text (format nil "op: insert, index: ~a, size: ~a" index size)))
    (ensure-array-list-capacity list (+ size 1))
    (when (< index size)
      (array-shift-right elements-array index (- size 1) 1))
    (setf (svref elements-array index) object)
    (incf size)))

(defmethod insert-object-after ((list array-list) (index abstract-iterator) object)
  (let ((iterator (it-native-iterator index)))
    (with-slots (current-index)
	iterator
      (insert-object-after list current-index object))))

(defmethod insert-object-after ((list array-list) (index fixnum) object)
  (insert-object-before list (+ index 1) object))

(defmethod insert-all-objects-before ((list array-list) (index abstract-iterator) (objects list))
  (let ((iterator (it-native-iterator index)))
    (with-slots (current-index)
	iterator
      (insert-all-objects-before list current-index objects)
      (incf current-index (length objects)))))

(defmethod insert-all-objects-before ((list array-list) (index abstract-iterator) (objects abstract-collection))
  (let ((iterator (it-native-iterator index)))
    (with-slots (current-index)
	iterator
      (insert-all-objects-before list current-index objects)
      (incf current-index (size objects)))))

(defmethod insert-all-objects-before ((list array-list) (index abstract-iterator) (iterator abstract-iterator))
  (loop while (it-next iterator)
	do (insert-object-before list index (it-current iterator))))

(defmethod insert-all-objects-before ((list array-list) (index fixnum) (iterator abstract-iterator))
  (loop for insert-index from index
	while (it-next iterator)
	do (insert-object-before list insert-index
				 (it-current iterator))))

(defmethod insert-all-objects-before ((list array-list) (index fixnum) (objects list))
  (with-slots (size elements-array)
      list
    (when (and (< index 0)
	       (> index size))
      (error 'list-index-out-of-bounds :text (format nil "op: insert, index: ~a, size: ~a" index size)))
    (let ((insert-size (length objects)))
      (ensure-array-list-capacity list (+ size insert-size))
      (when (< index size)
	(array-shift-right elements-array index (- size 1) insert-size))
      (loop
	 for object in objects
	 for insert-index from index
	 do (setf (svref elements-array insert-index) object))
      (incf size insert-size))))

(defmethod insert-all-objects-before ((list array-list) (index fixnum) (objects abstract-collection))
  (with-slots (size elements-array)
      list
    (when (and (< index 0)
	       (> index size))
      (error 'list-index-out-of-bounds :text (format nil "op: insert, index: ~a, size: ~a" index size)))
    (let ((insert-size (size objects)))
      (ensure-array-list-capacity list (+ size insert-size))
      (when (< index size)
	(array-shift-right elements-array index (- size 1) insert-size))
      (let ((iterator (iterator objects)))
	    (loop
	      for insert-index from index
	      while (it-next iterator)
	      do (setf (svref elements-array insert-index) (it-current iterator))))
      (incf size insert-size))))

(defmethod insert-all-objects-after ((list array-list) (index abstract-iterator) objects)
  (let ((iterator (it-native-iterator index)))
    (with-slots (current-index)
	iterator
      (insert-all-objects-after list current-index objects))))

(defmethod insert-all-objects-after ((list array-list) (index fixnum) objects)
  (insert-all-objects-before list (+ index 1) objects))

(defmethod remove-object-at ((list array-list) (index abstract-iterator))
  (let ((iterator (it-native-iterator index)))
    (with-slots (current-index prev-step)
	iterator
      (remove-object-at list current-index)
      (when (eq prev-step :STEP-FORWARD)
	(decf current-index)))))

(defmethod remove-object-at ((list array-list) (index fixnum))
  (with-slots (size elements-array)
      list
    (when (not (in-range-p list index))
      (error 'list-index-out-of-bounds :text (format nil "op: remove, index: ~a, size: ~a" index size)))
    (loop for i from index to (- size 2)
       do (setf (svref elements-array i)
		(svref elements-array (+ i 1))))
    (decf size)
    (setf (svref elements-array size) nil)))

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
