;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass hash-set (abstract-set)
  ((hash-map
    :initform nil
    :documentation "Back with hash-map."))
  (:documentation "Hashtable set."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((set hash-set) &key)
  (with-slots (test hash hash-map)
      set
    (setf hash-map (make-instance 'hash-map :test test :hash hash))))

(defmethod iterator ((set hash-set) &optional condition)
  (with-slots (hash-map)
      set
    (let ((iterator (make-instance 'hash-map-iterator
                                   :hash-map hash-map
                                   :extractor-fn (lambda (entry)
                                                   (slot-value entry 'key)))))
      (if condition
          (make-instance 'conditional-iterator :back-iterator iterator
                                               :condition condition)
          iterator))))

(defmethod size ((set hash-set))
  (with-slots (hash-map)
      set
    (size hash-map)))

(defmethod clear ((set hash-set))
  (with-slots (hash-map)
      set
    (clear hash-map)))

(defmethod add-object ((set hash-set) object)
  (with-slots (hash-map)
      set
    (put-object hash-map object nil)))

(defmethod remove-object ((set hash-set) object)
  (with-slots (hash-map)
      set
    (rem-object hash-map object)))

(defmethod remove-object ((set hash-set) (iterator abstract-iterator))
  (let ((native-iterator (it-native-iterator iterator)))
    (with-slots (hash-map)
        native-iterator
      (remove-object hash-map native-iterator))))

(defmethod member-object ((set hash-set) object)
  (with-slots (hash-map)
      set
    (contains-key hash-map object)))
