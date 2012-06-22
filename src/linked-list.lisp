;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(deftype linked-list-node () 'cons)

(defclass linked-list (abstract-list)
  ((size
    :type integer
    :initform 0
    :documentation "Size of list.")
   (before-first-node
    :type linked-list-node
    :initform nil
    :documentation "First entry of list.")
   (after-last-node
    :type linked-list-node
    :initform nil
    :documentation "Last entry of list."))
  (:documentation "List with elements linked each other."))

(defclass linked-list-iterator (abstract-iterator)
  ((linked-list
    :initarg :linked-list
    :type linked-list
    :initform (error "Specify linked-list."))
   (current-node
    :initarg :current-node
    :type linked-list-node))
  (:documentation "Iterator over linked list."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun linked-list-new-node ()
  (cons nil (cons nil nil)))

(defun linked-list-get-node (node)
  (declare (type cons node))
  (car node))

(defun linked-list-set-node (node object)
  (declare (type cons node))
  (setf (car node) object))

(defun linked-list-link-nodes (node1 node2)
  (declare (type cons node1 node2))
  (let ((refs-node-1 (cdr node1))
	(refs-node-2 (cdr node2)))
    (setf (cdr refs-node-1) refs-node-2)
    (setf (car refs-node-2) refs-node-1)))

(defun linked-list-add-first (list object)
  (declare (type linked-list list))
  (with-slots (before-first-node size)
      list
    (let ((next-node (cdr (cdr before-first-node)))
	  (new-node (linked-list-new-node)))
      (linked-list-set-node new-node object)
      (linked-list-link-nodes before-first-node new-node)
      (linked-list-link-nodes new-node next-node)
      (incf size))))

(defmethod initialize-instance :after ((list linked-list) &key)
  (with-slots (before-first-node after-last-node)
      list
    (let ((bf-node (linked-list-new-node))
	  (al-node (linked-list-new-node)))
      (setf before-first-node bf-node)
      (setf after-last-node al-node)
      (linked-list-link-nodes bf-node al-node))))

(defmethod size ((list linked-list))
  (slot-value list 'size))

(defmethod iterator ((list linked-list) &optional condition)
  (let ((iterator (make-instance 'linked-list-iterator :linked-list list)))
    (if condition
        (make-instance 'conditional-iterator :back-iterator iterator
					     :condition condition)
	iterator)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((iterator linked-list-iterator) &key)
  (with-slots (linked-list current-node)
      iterator
    (setf current-node (slot-value linked-list 'before-first-node))))

(defmethod it-current ((iterator linked-list-iterator))
  (with-slots (current-node)
      iterator
    (linked-list-get-node current-node)))

(defmethod it-next ((iterator linked-list-iterator))
  nil)
