;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(def-collection-impl-class graham-queue (abstract-queue)
  ((list
    :type list
    :initform '()
    :documentation "Back built-in list."))
  (:documentation "Graham queue."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod size ((queue graham-queue))
  (with-slots (list)
      queue
    (length list)))

(defmethod iterator ((queue graham-queue) &optional condition)
  (with-slots (list)
      queue
    (let ((iterator (make-instance 'built-in-list-iterator :list list)))
      (if condition
	  (make-instance 'conditional-iterator
			 :condition condition
			 :back-iterator iterator)
	  iterator))))

(defmethod clear ((queue graham-queue))
  (with-slots (list)
      queue
    (setf list '())))

(defmethod remove-object ((queue graham-queue) object)
  )

(defmethod peek-object ((queue graham-queue))
  )

(defmethod enqueue-object ((queue graham-queue) object))

(defmethod dequeue-object ((queue graham-queue))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
