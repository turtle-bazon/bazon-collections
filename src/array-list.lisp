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
    :documentation "Back array capaciy")
   (elements-array
    :type (array t)
    :initform nil
    :documentation "Back array"))
  (:documentation "List based on using arrays."))

(defmethod initialize-instance :after ((list array-list) &key)
  (with-slots (initial-capacity elements-array)
      list
    (setf elements-array (make-array initial-capacity :adjustable t))))

(defmethod size ((list array-list))
  (with-slots (size)
      list
    size))

(defmethod clear ((list array-list))
  (with-slots (size elements-array)
      list
    (loop for i from 0 below size
	 do (setf (aref elements-array i) nil))
    (setf size 0)))
