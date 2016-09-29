;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass graham-queue (abstract-queue)
  ((list-queue
    :type list
    :initform (cons nil nil)
    :documentation "Back built-in list."))
  (:documentation "Graham queue."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod size ((queue graham-queue))
  (with-slots (list-queue)
      queue
    (length (car list-queue))))

(defmethod empty-p ((queue graham-queue))
  (with-slots (list-queue)
      queue
    (null (car list-queue))))

(defmethod iterator ((queue graham-queue) &optional condition)
  (with-slots (list-queue)
      queue
    (let ((iterator (make-instance 'built-in-list-iterator :list (car list-queue))))
      (if condition
	  (make-instance 'conditional-iterator
			 :condition condition
			 :back-iterator iterator)
	  iterator))))

(defmethod clear ((queue graham-queue))
  (with-slots (list-queue)
      queue
    (setf list-queue (cons nil nil))))

(defmethod remove-object ((queue graham-queue) (index abstract-iterator))
  (let ((iterator (it-native-iterator index)))
    (with-slots (prev-cons current-cons)
	iterator
      (with-slots (list-queue)
	  queue
	(let ((next-cons (rest current-cons)))
	  (setf (cdr prev-cons) next-cons)
	  (setf current-cons prev-cons)
	  (when (eq (car prev-cons) :BEFORE-LIST)
	    (setf (car list-queue) next-cons))
	  (when (null next-cons)
	    (setf (cdr list-queue) prev-cons)))))))

(defmethod peek-object ((queue graham-queue))
  (with-slots (list-queue)
      queue
    (car (car list-queue))))

(defmethod enqueue-object ((queue graham-queue) object)
  (with-slots (list-queue)
      queue
    (if (null (car list-queue))
	(setf (cdr list-queue) (setf (car list-queue) (list object)))
	(setf (cdr (cdr list-queue)) (list object)
	      (cdr list-queue) (cdr (cdr list-queue))))))

(defmethod dequeue-object ((queue graham-queue))
  (with-slots (list-queue)
      queue
    (pop (car list-queue))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
