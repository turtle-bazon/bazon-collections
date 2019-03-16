;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass hash-set (abstract-set)
  ((hash-table
    :initform (make-hash-table)
    :documentation "Back built-in hash-table."))
  (:documentation "Hashtable set."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod size ((set hash-set))
  (with-slots (hash-table)
      set
    (hash-table-count hash-table)))

(defmethod clear ((set hash-set))
  (with-slots (hash-table)
      set
    (clrhash hash-table)))

(defmethod add-object ((set hash-set) object)
  (with-slots (hash-table)
      set
    (setf (gethash object hash-table) t)))

(defmethod remove-object ((set hash-set) object)
  (with-slots (hash-table)
      set
    (remhash object hash-table)))

(defmethod member-object ((set hash-set) object)
  (with-slots (hash-table)
      set
    (gethash object set)))
