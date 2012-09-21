;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections-tests)

(defun test-map-put-objects (map-class constructor-function element-function de-e)
  (let ((map (funcall constructor-function)))
    (ensure-same 0 (size map)
		 :report (report-name map-class "0 at beginning."))
    (put-object map (funcall element-function 1) (funcall element-function 100))
    (put-object map (funcall element-function 2) (funcall element-function 200))
    (put-object map (funcall element-function 1) (funcall element-function 101))
    (ensure-same 2 (size map)
		 :report (report-name map-class "2 elements after add 1, 2, 1"))
    (ensure-same 101 (funcall de-e (get-object map (funcall element-function 1)))
		 :report (report-name map-class "101 is under 1"))
    (ensure-same 200 (funcall de-e (get-object map (funcall element-function 2)))
		 :report (report-name map-class "200 is under 2"))))

(defun test-map (map-class constructor-function element-function de-e)
  (test-map-put-objects map-class constructor-function element-function de-e))
