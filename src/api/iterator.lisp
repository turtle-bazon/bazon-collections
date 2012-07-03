;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass abstract-iterator ()
  ()
  (:documentation "Common iterator interface."))

(def-r-generic it-native-iterator (iterator)
  (:documentation "Return native (begin of begin and collection specific) iterator in possible decorated iterator. Returns self for native collection iterators."))

(def-r-generic it-current (iterator)
  (:documentation "Access to current object in iterator."))

(def-w-generic it-next (iterator)
  (:documentation "Iterate to next object and returns if iterate result."))

(def-w-generic it-prev (iterator)
  (:documentation "Iterate to previous object and returns iterate result."))

(def-w-generic it-before-first (iterator)
  (:documentation "Move cursor before first object."))

(def-w-generic it-after-last (iterator)
  (:documentation "Move cursor after last object."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod it-native-iterator ((iterator abstract-iterator))
  iterator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass abstract-decorating-iterator (abstract-iterator)
  ((back-iterator
    :initarg :back-iterator
    :initform (error "Specify back iterator.")
    :type abstract-iterator
    :documentation "Back iterator without condition."))
  (:documentation "Abstract decorating iterator over another iterator."))

(defmethod it-native-iterator ((iterator abstract-decorating-iterator))
  (it-native-iterator (slot-value iterator 'back-iterator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass conditional-iterator (abstract-decorating-iterator)
  ((condition
    :initarg :condition
    :initform (error "Specify iterator condition.")
    :documentation "Function of one argument returning true or false."))
  (:documentation "Iterator that iterates to object by given condition."))

(defmethod it-current ((iterator conditional-iterator))
  (with-slots (back-iterator)
      iterator
    (it-current back-iterator)))

(defmethod it-next ((iterator conditional-iterator))
  (with-slots (back-iterator condition)
      iterator
    (loop while (it-next back-iterator)
	  when (funcall condition (it-current back-iterator))
	    return t)))

(defmethod it-prev ((iterator conditional-iterator))
  (with-slots (back-iterator condition)
      iterator
    (loop while (it-prev back-iterator)
	  when (funcall condition (it-current back-iterator))
	    return t)))

(defmethod it-before-first ((iterator conditional-iterator))
  (with-slots (back-iterator)
      iterator
    (it-before-first back-iterator)))

(defmethod it-after-last ((iterator conditional-iterator))
  (with-slots (back-iterator)
      iterator
    (it-after-last back-iterator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass built-in-list-iterator (abstract-iterator)
  ((current-cons
    :initarg :list
    :initform (error "Define current-cons.")
    :type list)
   (prev-cons
    :type cons
    :initform nil))
  (:documentation "Iterator over built-in list type (only forward)."))

(defmethod initialize-instance :after ((iterator built-in-list-iterator) &key list)
  (with-slots (current-cons)
      iterator
    (setf current-cons (cons :BEFORE-LIST list))))

(defmethod it-current ((iterator built-in-list-iterator))
  (with-slots (current-cons)
      iterator
    (if (eq current-cons '())
	:AFTER-LIST
	(car current-cons))))

(defmethod it-next ((iterator built-in-list-iterator))
  (with-slots (current-cons prev-cons)
      iterator
    (setf prev-cons current-cons)
    (setf current-cons (rest current-cons))
    (not (eq current-cons '()))))
