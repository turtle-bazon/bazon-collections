;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass abstract-map ()
  ((test
    :initarg :test
    :initform #'equal
    :documentation "Comparator function for contained keys.")
   (hash
    :initarg :hash
    :initform #'sxhash
    :documentation "Hash function for contained keys."))
  (:documentation "Collection that maps keys to values."))

(defgeneric get-keys (map)
  (:documentation "Get keys contained in map."))

(defgeneric get-values (map)
  (:documentation "Get values contained in map."))

(defgeneric contains-key (map object)
  (:documentation "Tests if object contains as key in map."))

(defgeneric contains-value (map object)
  (:documentation "Tests if object contains as value in map."))

(defgeneric get-object (map key)
  (:documentation "Gets value by key."))

(defgeneric rem-object (map key)
  (:documentation "Remove value by key."))

(defgeneric put-object (map key value)
  (:documentation "Puts key-value pair."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass map-entry ()
  ((key
    :initarg :key
    :initform (error "Entry key must be defined")
    :documentation "Key element of map entry")
   (value
    :initarg :value
    :initform (error "Entry value must be defined")
    :documentation "Value element of map entry"))
  (:documentation "Map entry that holds key and value"))

(defgeneric map-entry-key (me)
  (:documentation "Entry key")
  (:method (me)
    (slot-value me 'key)))

(defgeneric map-entry-value (me)
  (:documentation "Entry value")
  (:method (me)
    (slot-value me 'value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod oequal-p ((map abstract-map) o1 o2)
  (with-slots (test hash)
      map
    (and o1 o2
         (= (funcall hash o1)
            (funcall hash o2))
         (funcall test o1 o2))))

(defmethod empty-p ((map abstract-map))
  (= 0 (size map)))
