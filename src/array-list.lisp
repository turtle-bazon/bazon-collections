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

(defun ensure-array-list-capacity (list need-size)
  (with-slots (elements-array)
      list
    (let* ((current-size (length elements-array))
	   (size-diff (- need-size current-size)))
      (when (plusp size-diff)
	(let* ((size-addition (* current-size 2))
	       (new-array (make-array (+ current-size size-addition))))
	  (dotimes (i current-size)
	    (setf (aref new-array i)
		  (aref elements-array i)))
	  (setf elements-array new-array))))))

(defmethod add-object ((list array-list) object)
  (with-slots (size elements-array)
      list
    (let* ((new-size (+ size 1)))
      (ensure-array-list-capacity list new-size)
      (setf (aref elements-array size) object)
      (setf size new-size))))