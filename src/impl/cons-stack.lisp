;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass cons-stack (abstract-stack)
  ((list
    :type list
    :initform '()
    :documentation "Back built-in list."))
  (:documentation "Cons stack."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod size ((stack cons-stack))
  (with-slots (list)
      stack
    (length list)))

(defmethod empty-p ((stack cons-stack))
  (with-slots (list)
      stack
    (null list)))

(defmethod iterator ((stack cons-stack) &optional condition)
  (with-slots (list)
      stack
    (let ((iterator (make-instance 'built-in-list-iterator :list list)))
      (if condition
	  (make-instance 'conditional-iterator
			 :condition condition
			 :back-iterator iterator)
	  iterator))))

(defmethod clear ((stack cons-stack))
  (with-slots (list)
      stack
    (setf list '())))

(defmethod remove-object ((stack cons-stack) (index abstract-iterator))
  (let ((iterator (it-native-iterator index)))
    (with-slots (current-cons prev-cons)
	iterator
      (with-slots (list)
	  stack
	(let ((next-cons (rest current-cons)))
	  (setf (cdr prev-cons) next-cons)
	  (setf current-cons prev-cons)
	  (when (eq (car prev-cons) :BEFORE-LIST)
	    (setf list next-cons)))))))

(defmethod peek-object ((stack cons-stack))
  (with-slots (list)
      stack
    (car list)))

(defmethod push-object ((stack cons-stack) object)
  (with-slots (list)
      stack
    (push object list)))

(defmethod pop-object ((stack cons-stack))
  (with-slots (list)
      stack
    (pop list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
