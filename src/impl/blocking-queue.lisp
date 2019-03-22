;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass blocking-queue (abstract-queue)
  ((back-queue
    :type abstract-queue
    :initarg :back-queue
    :initform (error "Need to specify back-queue implementation.")
    :documentation "Blocking queue.")
   (lock
    :initform (make-lock))
   (increased-condition
    :initform (make-condition-variable :name "increased-condition"))
   (decreased-condition
    :initform (make-condition-variable :name "decreased-condition")))
  (:documentation "Blocking queue."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-blocking-queue (implementation-class)
  (make-instance 'blocking-queue :back-queue (make-instance implementation-class)))

(defmethod size ((queue blocking-queue))
  (with-slots (back-queue lock)
      queue
    (with-lock-held (lock)
      (size back-queue))))

(defmethod iterator ((queue blocking-queue) &optional condition)
  (with-slots (back-queue)
      queue
    (iterator back-queue condition)))

(defmethod clear ((queue blocking-queue))
  (with-slots (back-queue lock decreased-condition)
      queue
    (with-lock-held (lock)
      (let ((clear-result (clear back-queue)))
        (condition-notify decreased-condition)
        clear-result))))

(defmethod remove-object ((queue blocking-queue) (index abstract-iterator))
  (with-slots (back-queue lock decreased-condition)
      queue
    (with-lock-held (lock)
      (let ((remove-result (remove-object back-queue)))
        (condition-notify decreased-condition)
        remove-result))))

(defmethod peek-object ((queue blocking-queue))
  (with-slots (back-queue lock)
      queue
    (with-lock-held (lock)
      (peek-object back-queue))))

(defmethod enqueue-object ((queue blocking-queue) object)
  (with-slots (back-queue lock increased-condition decreased-condition)
      queue
    (with-lock-held (lock)
      (loop while (full-p back-queue)
         do (condition-wait decreased-condition lock))
      (let ((enque-result (enqueue-object back-queue object)))
        (condition-notify increased-condition)
        enque-result))))

(defmethod dequeue-object ((queue blocking-queue))
  (with-slots (back-queue lock increased-condition decreased-condition)
      queue
    (with-lock-held (lock)
      (loop while (empty-p back-queue)
         do (condition-wait increased-condition lock))
      (let ((dequeue-result (dequeue-object back-queue)))
        (condition-notify decreased-condition)
        dequeue-result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
