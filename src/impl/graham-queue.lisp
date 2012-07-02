;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(def-collection-impl-class graham-queue (abstract-queue)
  ((list
    :type list
    :initform '()
    :documentation "Back built-in list."))
  (:documentation "Graham queue."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
