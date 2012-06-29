;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass simple-queue (abstract-queue)
  ((linked-list
    :type linked-list
    :initform (make-instance 'linked-list)
    :documentation "Back linked list."))
  (:documentation "Simple queue."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod size ((queue simple-queue))
  (with-slots (linked-list)
      queue
    (size linked-list)))

(defmethod iterator ((queue simple-queue) &optional condition)
  (with-slots (linked-list)
      queue
    (iterator linked-list condition)))

(defmethod clear ((queue simple-queue))
  (with-slots (linked-list)
      queue
    (clear linked-list)))

(defmethod remove-object ((queue simple-queue) (index abstract-iterator))
  (with-slots (linked-list)
      queue
    (remove-object-at linked-list index)))

(defmethod peek-object ((queue simple-queue))
  (with-slots (linked-list)
      queue
    (with-slots (size before-first-node)
	linked-list
      (when (> size 0)
	(linked-list-node-object
	 (linked-list-next-node before-first-node))))))

(defmethod enqueue-object ((queue simple-queue) object)
  (with-slots (linked-list)
      queue
    (linked-list-add-object-last linked-list object)))

(defmethod dequeue-object ((queue simple-queue))
  (with-slots (linked-list)
      queue
    (linked-list-remove-object-first linked-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

