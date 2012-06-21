;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass linked-list (abstract-list)
  ((first-entry
    :type cons
    :initform nil
    :documentation "First entry of list.")
   (last-entry
    :type cons
    :initform nil
    :documentation "Last entry of list.")
   (size
    :type integer
    :initform 0
    :documentation "Size of list."))
  (:documentation "List with elements linked each other."))

(defclass linked-list-iterator (abstract-iterator)
  ((linked-list
    :initarg :linked-list
    :type linked-list))
  (:documentation "Iterator over linked list."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

