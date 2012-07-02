;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass abstract-map (abstract-collection)
  ()
  (:documentation "Collection that maps keys to values."))

(def-r-generic get-keys (map)
  (:documentation "Get keys contained in map."))

(def-r-generic get-values (map)
  (:documentation "Get values contained in map."))

(def-r-generic contains-key (map object)
  (:documentation "Tests if object contains as key in map."))

(def-r-generic contains-value (map object)
  (:documentation "Tests if object contains as value in map."))

(def-r-generic get-object (map key)
  (:documentation "Gets valu by key."))

(def-w-generic put-object (map key value)
  (:documentation "Puts key-value pair."))
