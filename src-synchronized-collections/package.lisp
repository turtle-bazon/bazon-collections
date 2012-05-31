;;; -*- lisp -*-

(defpackage :ru.bazon.bazon-synchronized-collections
  (:nicknames :bazon-synchronized-collections)
  (:use :cl
	:bazon-collections
	:bordeaux-threads)
  (:documentation "Common Lisp Collections framework (synchronized extension)"))

(in-package :ru.bazon.bazon-synchronized-collections)

(defclass synchronized-object ()
  ((lock :initform (make-lock)))
  (:documentation ""))

(dolist (collection-impl (collection-impls))
  (let ((synchronized-collection-impl (with-input-from-string (sstream (format nil "synchronized-~a" collection-impl))
					(read sstream))))
    (eval
     `(defclass ,synchronized-collection-impl (synchronized-object ,collection-impl) ()))
    (export synchronized-collection-impl)))

(maphash
 (lambda (key value)
   (when (eq :write (getf value :context))
     (eval
      `(defmethod ,key ,(cons '(collection synchronized-object) (rest (getf value :params)))
	 (with-slots (lock)
	     collection
	   (with-lock-held (lock)
	     (call-next-method)))))))
 (generic-marks))
