;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass abstract-map (abstract-collection)
  ()
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
  (:documentation "Gets valu by key."))

(defgeneric put-object (map key value)
  (:documentation "Puts key-value pair."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun entry-key (entry)
  (car entry))

(defun entry-value (entry)
  (cdr entry))
