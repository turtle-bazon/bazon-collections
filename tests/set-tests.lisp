;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections-tests)

(defun test-set-add-objects (set-class constructor-function element-function de-e)
  (let ((set (funcall constructor-function)))
    (ensure-same 0 (size set)
		 :report (report-name set-class "0 at beginning."))
    (add-object set (funcall element-function 1))
    (add-object set (funcall element-function 2))
    (add-object set (funcall element-function 1))
    (ensure-same 2 (size set)
		 :report (report-name set-class "2 elements after add 1, 2, 1"))
    (let ((iterator (c-it (iterator set))))
      (when (it-next iterator)
	(ensure-same 3 (+ (funcall de-e (it-current iterator))
			  (if (it-next iterator)
			      (funcall de-e (it-current iterator))
			      0))
		     :report (report-name set-class "Sum of elements is 3 after add 1, 2, 1"))
	(ensure-same nil (it-next iterator)
		     :report (report-name set-class "No more elements after iterate set after adding 1, 2, 1"))))))

(defun test-set (set-class constructor-function element-function de-e)
  (test-set-add-objects set-class constructor-function element-function de-e))
