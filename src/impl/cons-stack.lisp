;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(def-collection-impl-class cons-stack (abstract-stack)
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

(defmethod remove-object ((stack cons-stack) object)
  )

(defmethod peek-object ((stack cons-stack))
  )

(defmethod push-object ((stack cons-stack) object)
  )

(defmethod pop-object ((stack cons-stack))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
