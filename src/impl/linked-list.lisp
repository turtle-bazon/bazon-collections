;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(deftype linked-list-node () 'cons)

(def-collection-impl-class linked-list (abstract-list)
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
    :type linked-list-node)
   (previous-node
    :type linked-list-node))
  (:documentation "Iterator over linked list."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun linked-list-link-nodes (node1 node2)
  (declare (type linked-list-node node1 node2))
  (setf (cdr (cdr node1)) node2)
  (setf (car (cdr node2)) node1))

(defun linked-list-node-object (node)
  (declare (type linked-list-node node))
  (car node))

(defun linked-list-prev-node (node)
  (declare (type linked-list-node node))
  (car (cdr node)))

(defun linked-list-next-node (node)
  (declare (type linked-list-node node))
  (cdr (cdr node)))

(defun linked-list-add-object-first (linked-list object)
  (declare (type linked-list linked-list))
  (with-slots (size before-first-node)
      linked-list
    (let ((new-node (cons object (cons nil nil)))
	  (next-node (linked-list-next-node before-first-node)))
      (linked-list-link-nodes before-first-node new-node)
      (linked-list-link-nodes new-node next-node)
      (incf size))))

(defun linked-list-add-object-last (linked-list object)
  (declare (type linked-list linked-list))
  (with-slots (size after-last-node)
      linked-list
    (let ((new-node (cons object (cons nil nil)))
	  (prev-node (linked-list-prev-node after-last-node)))
      (linked-list-link-nodes prev-node new-node)
      (linked-list-link-nodes new-node after-last-node)
      (incf size))))

(defun linked-list-remove-object-first (linked-list)
  (declare (type linked-list linked-list))
  (with-slots (size before-first-node)
      linked-list
    (when (> size 0)
      (let* ((next-node (linked-list-next-node before-first-node))
	     (after-next-node (linked-list-next-node next-node)))
	(linked-list-link-nodes before-first-node after-next-node)
	(decf size)
	(linked-list-node-object next-node)))))

(defun linked-list-remove-object-last (linked-list)
  (declare (type linked-list linked-list))
  (with-slots (size after-last-node)
      linked-list
    (when (> size 0)
      (let* ((prev-node (linked-list-prev-node after-last-node))
	     (before-prev-node (linked-list-prev-node prev-node)))
	(linked-list-link-nodes before-prev-node after-last-node)
	(decf size)
	(linked-list-node-object prev-node)))))

(defun linked-list-move-iterator-to (iterator index)
  (declare (type linked-list-iterator iterator)
	   (type integer index))
  (it-before-first iterator)
  (loop for i from 0 to index
     do (it-next iterator)))

(defmethod initialize-instance :after ((list linked-list) &key)
  (with-slots (before-first-node after-last-node)
      list
    (setf before-first-node (cons nil (cons nil nil)))
    (setf after-last-node (cons nil (cons nil nil)))
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
  (with-slots (size before-first-node after-last-node)
      list
    (linked-list-link-nodes before-first-node after-last-node)
    (setf size 0)))

(defmethod add-object ((list linked-list) object)
  (linked-list-add-object-last list object))

(defmethod add-all-objects ((list linked-list) objects)
  (let ((iterator (iterator list)))
    (it-after-last iterator)
    (insert-all-objects-before list iterator objects)))

(defmethod get-object-at ((list linked-list) (index integer))
  (when (not (in-range-p list index))
    (error 'list-index-out-of-bounds (format nil "op: get-object-at, index: ~a, size: ~a"
					     index (slot-value list 'size))))
  (let ((iterator (iterator list)))
    (linked-list-move-iterator-to iterator index)
    (it-current iterator)))

(defmethod set-object-at ((list linked-list) (index integer) object)
  (when (not (in-range-p list index))
    (error 'list-index-out-of-bounds (format nil "op: set-object-at, index: ~a, size: ~a"
					     index (slot-value list 'size))))
  (let ((iterator (iterator list)))
    (linked-list-move-iterator-to iterator index)
    (set-object-at list iterator object)))

(defmethod set-object-at ((list linked-list) (index abstract-iterator) object)
  (let ((iterator (it-native-iterator index)))
    (with-slots (current-node)
	iterator
      (with-slots (size before-first-node after-last-node)
	  list
	(when (or (eq current-node before-first-node)
		  (eq current-node after-last-node))
	  (error 'list-index-out-of-bounds (format nil "op: set-object-at, index: ~a, size: ~a"
						   index size)))
	(setf (car current-node) object)))))

(defmethod insert-object-before ((list linked-list) (index integer) object)
  (with-slots (size)
      list
    (when (not (or (in-range-p list index)
		   (= index size)))
      (error 'list-index-out-of-bounds (format nil "op: insert-object-before, index: ~a, size: ~a"
					       index size)))
    (let ((iterator (iterator list)))
      (linked-list-move-iterator-to iterator index)
      (insert-object-before list iterator object))))

(defmethod insert-object-before ((list linked-list) (index abstract-iterator) object)
  (let ((iterator (it-native-iterator index)))
    (with-slots (current-node)
	iterator
      (with-slots (size before-first-node after-last-node)
	  list
	(when (eq current-node before-first-node)
	  (error 'list-index-out-of-bounds (format nil "op: insert-object-before, index: ~a, size: ~a"
						   index size)))
	(let ((new-node (cons object (cons nil nil)))
	      (prev-node (car (cdr current-node))))
	  (linked-list-link-nodes prev-node new-node)
	  (linked-list-link-nodes new-node current-node)
	  (incf size))))))

(defmethod insert-object-after ((list linked-list) (index integer) object)
  (with-slots (size)
      list
    (when (not (or (in-range-p list index)
		   (= index -1)))
      (error 'list-index-out-of-bounds (format nil "op: insert-object-after, index: ~a, size: ~a"
					       index size)))
    (let ((iterator (iterator list)))
      (linked-list-move-iterator-to iterator index)
      (insert-object-after list iterator object))))

(defmethod insert-object-after ((list linked-list) (index abstract-iterator) object)
  (let ((iterator (it-native-iterator index)))
    (with-slots (current-node)
	iterator
      (with-slots (size before-first-node after-last-node)
	  list
	(when (eq current-node after-last-node)
	  (error 'list-index-out-of-bounds (format nil "op: insert-object-after, index: ~a, size: ~a"
						   index size)))
	(let ((new-node (cons object (cons nil nil)))
	      (next-node (cdr (cdr current-node))))
	  (linked-list-link-nodes current-node new-node)
	  (linked-list-link-nodes new-node next-node)
	  (incf size))))))

(defmethod insert-all-objects-before ((list linked-list) (index integer) objects)
  (with-slots (size)
      list
    (when (not (or (in-range-p list index)
		   (= index size)))
      (error 'list-index-out-of-bounds (format nil "op: insert-all-objects-before, index: ~a, size: ~a" index size)))
    (let ((iterator (iterator list)))
      (linked-list-move-iterator-to iterator index)
      (insert-all-objects-before list iterator objects))))

(defmethod insert-all-objects-before ((list linked-list) (index abstract-iterator) (objects list))
  (let ((iterator (make-instance 'built-in-list-iterator :list objects)))
    (insert-all-objects-before list index iterator)))

(defmethod insert-all-objects-before ((list linked-list) (index abstract-iterator) (objects abstract-collection))
  (let ((iterator (iterator objects)))
    (insert-all-objects-before list index iterator)))

(defmethod insert-all-objects-before ((list linked-list) (index abstract-iterator) (objects abstract-iterator))
  (let ((iterator (it-native-iterator index)))
    (with-slots (current-node)
	iterator
      (with-slots (size before-first-node)
	  list
	(when (eq current-node before-first-node)
	  (error 'list-index-out-of-bounds (format nil "op: insert-all-objects-before, index: ~a, size: ~a" index size)))
	(loop while (it-next objects)
	   do (let ((new-node (cons (it-current objects) (cons nil nil)))
		    (prev-node (car (cdr current-node))))
		(linked-list-link-nodes prev-node new-node)
		(linked-list-link-nodes new-node current-node)
		(incf size)))))))

(defmethod insert-all-objects-after ((list linked-list) (index integer) objects)
  (with-slots (size)
      list
    (when (not (or (in-range-p list index)
		   (= index -1)))
      (error 'list-index-out-of-bounds (format nil "op: insert-all-objects-after, index: ~a, size: ~a" index size)))
    (let ((iterator (iterator list)))
      (linked-list-move-iterator-to iterator index)
      (insert-all-objects-after list iterator objects))))

(defmethod insert-all-objects-after ((list linked-list) (index abstract-iterator) (objects list))
  (let ((iterator (make-instance 'built-in-list-iterator :list objects)))
    (insert-all-objects-after list index iterator)))

(defmethod insert-all-objects-after ((list linked-list) (index abstract-iterator) (objects abstract-collection))
  (let ((iterator (iterator objects)))
    (insert-all-objects-after list index iterator)))

(defmethod insert-all-objects-after ((list linked-list) (index abstract-iterator) (objects abstract-iterator))
  (let ((iterator (it-native-iterator index)))
    (with-slots (current-node)
	iterator
      (with-slots (size after-last-node)
	  list
	(when (eq current-node after-last-node)
	  (error 'list-index-out-of-bounds (format nil "op: insert-all-objects-before, index: ~a, size: ~a" index size)))
	(let ((next-node (cdr (cdr current-node))))
	  (loop while (it-next objects)
		do (let ((new-node (cons (it-current objects) (cons nil nil)))
			 (prev-node (car (cdr next-node))))
		     (linked-list-link-nodes prev-node new-node)
		     (linked-list-link-nodes new-node next-node)
		     (incf size))))))))

(defmethod remove-object-at ((list linked-list) (index integer))
  (with-slots (size)
      list
    (when (not (in-range-p list index))
      (error 'list-index-out-of-bounds (format nil "op: remove-object-at, index: ~a, size: ~a" index size)))
    (let ((iterator (iterator list)))
      (linked-list-move-iterator-to iterator index)
      (remove-object-at list iterator))))

(defmethod remove-object-at ((list linked-list) (index abstract-iterator))
  (let ((iterator (it-native-iterator index)))
    (with-slots (size before-first-node after-last-node)
	list
      (with-slots (current-node previous-node)
	  iterator
	(when (or (eq current-node before-first-node)
		  (eq current-node after-last-node))
	  (error 'list-index-out-of-bounds (format nil "op: remove-object-at, index: ~a, size: ~a" index size)))
	(let ((prev-node (car (cdr current-node)))
	      (next-node (cdr (cdr current-node))))
	  (linked-list-link-nodes prev-node next-node)
	  (setf current-node previous-node)
	  (decf size))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((iterator linked-list-iterator) &key)
  (with-slots (linked-list current-node)
      iterator
    (setf current-node (slot-value linked-list 'before-first-node))))

(defmethod it-current ((iterator linked-list-iterator))
  (with-slots (current-node)
      iterator
    (car current-node)))

(defmethod it-next ((iterator linked-list-iterator))
  (with-slots (linked-list current-node previous-node)
      iterator
    (with-slots (after-last-node)
	linked-list
      (setf previous-node current-node)
      (setf current-node (cdr (cdr current-node)))
      (not (eq current-node after-last-node)))))

(defmethod it-prev ((iterator linked-list-iterator))
  (with-slots (linked-list current-node previous-node)
      iterator
    (with-slots (before-first-node)
	linked-list
      (setf previous-node current-node)
      (setf current-node (car (cdr current-node)))
      (not (eq current-node before-first-node)))))

(defmethod it-before-first ((iterator linked-list-iterator))
  (with-slots (linked-list current-node previous-node)
      iterator
    (with-slots (before-first-node)
	linked-list
      (setf previous-node current-node)
      (setf current-node before-first-node))))

(defmethod it-after-last ((iterator linked-list-iterator))
  (with-slots (linked-list current-node previous-node)
      iterator
    (with-slots (after-last-node)
	linked-list
      (setf previous-node current-node)
      (setf current-node after-last-node))))
