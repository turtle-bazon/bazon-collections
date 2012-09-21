;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass list-set (abstract-set)
  ((list
    :type list
    :initform '()
    :documentation "Back built-in list."))
  (:documentation "List set."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod size ((set list-set))
  (with-slots (list)
      set
    (length list)))

(defmethod empty-p ((set list-set))
  (with-slots (list)
      set
    (null list)))

(defmethod iterator ((set list-set) &optional condition)
  (with-slots (list)
      set
    (let ((iterator (make-instance 'built-in-list-iterator :list list)))
      (if condition
	  (make-instance 'conditional-iterator
			 :condition condition
			 :back-iterator iterator)
	  iterator))))

(defmethod clear ((set list-set))
  (with-slots (list)
      set
    (setf list '())))

(defmethod add-object ((set list-set) object)
  (when (not (contains set object))
    (with-slots (list)
	set
      (push object list))))

(defmethod remove-object ((set list-set) (index abstract-iterator))
  (let ((iterator (it-native-iterator index)))
    (with-slots (current-cons prev-cons)
	iterator
      (with-slots (list)
	  set
	(let ((next-cons (rest current-cons)))
	  (setf (cdr prev-cons) next-cons)
	  (setf current-cons prev-cons)
	  (when (eq (car prev-cons) :BEFORE-LIST)
	    (setf list next-cons)))))))

(defmethod member-object ((set list-set) object)
  (contains set object))
