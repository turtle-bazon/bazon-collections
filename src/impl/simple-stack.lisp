;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(def-collection-impl-class simple-stack (abstract-stack)
  ((linked-list
    :type linked-list
    :initform (make-instance 'linked-list)
    :documentation "Back linked list."))
  (:documentation "Simple stack."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod size ((stack simple-stack))
  (with-slots (linked-list)
      stack
    (size linked-list)))

(defmethod iterator ((stack simple-stack) &optional condition)
  (with-slots (linked-list)
      stack
    (iterator linked-list condition)))

(defmethod clear ((stack simple-stack))
  (with-slots (linked-list)
      stack
    (clear linked-list)))

(defmethod remove-object ((stack simple-stack) (index abstract-iterator))
  (with-slots (linked-list)
      stack
    (remove-object-at linked-list index)))

(defmethod peek-object ((stack simple-stack))
  (with-slots (linked-list)
      stack
    (with-slots (size before-first-node)
	linked-list
      (when (> size 0)
	(linked-list-node-object
	 (linked-list-next-node before-first-node))))))

(defmethod push-object ((stack simple-stack) object)
  (with-slots (linked-list)
      stack
    (linked-list-add-object-first linked-list object)))

(defmethod pop-object ((stack simple-stack))
  (with-slots (linked-list)
      stack
    (linked-list-remove-object-first linked-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
