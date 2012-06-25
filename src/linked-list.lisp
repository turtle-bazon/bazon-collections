;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass linked-list-node ()
  ((object
    :initform nil
    :documentation "Stored object")
   (next-node
    :type linked-list-entry
    :initform nil
    :documentation "Link to next entry node.")
   (prev-node
    :type linked-list-entry
    :initform nil
    :documentation "Link to previous entry node."))
  (:documentation "Entry of linked list."))

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

(defun linked-list-link-nodes (node1 node2)
  (declare (type linked-list-node node1 node2))
  (setf (slot-value node1 'next-node) node2)
  (setf (slot-value node2 'prev-node) node1))

(defun linked-list-add-first (list object)
  (declare (type linked-list list))
  (with-slots (before-first-node size)
      list
    (let ((next-node (slot-value before-first-node 'next-node))
	  (new-node (make-instance 'linked-list-node)))
      (setf (slot-value new-node 'object) object)
      (linked-list-link-nodes before-first-node new-node)
      (linked-list-link-nodes new-node next-node)
      (incf size))))

(defmethod initialize-instance :after ((list linked-list) &key)
  (with-slots (before-first-node after-last-node)
      list
    (setf before-first-node (make-instance 'linked-list-node))
    (setf after-last-node (make-instance 'linked-list-node))
    (linked-list-link-nodes before-first-node after-last-node)))

(defmethod size ((list linked-list))
  (slot-value list 'size))

(defmethod iterator ((list linked-list) &optional condition)
  (let ((iterator (make-instance 'linked-list-iterator :linked-list list)))
    (if condition
        (make-instance 'conditional-iterator :back-iterator iterator
					     :condition condition)
	iterator)))

(defmethod clear ((list linked-list))
  (with-slots (before-first-node after-last-node)
      list
    (linked-list-link-nodes before-first-node after-last-node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((iterator linked-list-iterator) &key)
  (with-slots (linked-list current-node)
      iterator
    (setf current-node (slot-value linked-list 'before-first-node))))

(defmethod it-current ((iterator linked-list-iterator))
  (with-slots (current-node)
      iterator
    (slot-value current-node 'object)))

(defmethod it-next ((iterator linked-list-iterator))
  (with-slots (current-node)
      iterator
    (setf current-node (slot-value current-node 'next-node))))

(defmethod it-prev ((iterator linked-list-iterator))
  (with-slots (current-node)
      iterator
    (setf current-node (slot-value current-node 'prev-node))))

(defmethod it-before-first ((iterator linked-list-iterator))
  (with-slots (linked-list current-node)
      iterator
    (with-slots (before-first-node)
	linked-list
      (setf current-node before-first-node))))

(defmethod it-after-last ((iterator linked-list-iterator))
  (with-slots (linked-list current-node)
      iterator
    (with-slots (after-last-node)
	linked-list
      (setf current-node after-last-node))))
