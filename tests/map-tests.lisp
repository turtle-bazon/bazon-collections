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
		 :report (report-name map-class "200 is under 2"))
    (rem-object map (funcall element-function 1))
    (ensure-same 1 (size map))
    (ensure-same 200 (funcall de-e (get-object map (funcall element-function 2)))
                 :report (report-name map-class "200 is under 2"))))

(defun test-map-remove (map-class constructor-function element-function de-e)
  (let ((map (funcall constructor-function)))
    (ensure-same 0 (size map)
                 :report (report-name map-class "0 at beginning."))
    (put-object map (funcall element-function 1) (funcall element-function 100))
    (put-object map (funcall element-function 2) (funcall element-function 200))
    (rem-object map (funcall element-function 1))
    (ensure-same 1 (size map))
    (ensure-same nil (funcall de-e (get-object map (funcall element-function 1)))
                 :report (report-name map-class "nil under 1 now"))
    (rem-object map (funcall element-function 1))
    (ensure-same 1 (size map)
                 :report (report-name map-class "nothing changed after removing by non existing key"))))

(defun test-map-clear (map-class constructor-function element-function)
  (let ((map (funcall constructor-function)))
    (ensure-same 0 (size map)
                 :report (report-name map-class "0 at beginning."))
    (put-object map (funcall element-function 1) (funcall element-function 100))
    (put-object map (funcall element-function 2) (funcall element-function 200))
    (clear map)
    (ensure-same 0 (size map)
                 :report (report-name map-class "size 0 after clear"))
    (ensure-same t (empty-p map)
                 :report (report-name map-class "empty after clear"))))

(defun test-map-clear-after-grow (map-class constructor-function element-function)
  (let ((map (funcall constructor-function)))
    (ensure-same 0 (size map)
                 :report (report-name map-class "0 at beginning."))
    (put-object map (funcall element-function 1) (funcall element-function 100))
    (put-object map (funcall element-function 2) (funcall element-function 200))
    (put-object map (funcall element-function 3) (funcall element-function 300))
    (put-object map (funcall element-function 4) (funcall element-function 400))
    (put-object map (funcall element-function 5) (funcall element-function 500))
    (put-object map (funcall element-function 6) (funcall element-function 600))
    (put-object map (funcall element-function 7) (funcall element-function 700))
    (put-object map (funcall element-function 8) (funcall element-function 800))
    (put-object map (funcall element-function 9) (funcall element-function 900))
    (put-object map (funcall element-function 10) (funcall element-function 1000))
    (put-object map (funcall element-function 11) (funcall element-function 1100))
    (put-object map (funcall element-function 12) (funcall element-function 1200))
    (clear map)
    (ensure-same 0 (size map)
                 :report (report-name map-class "size 0 after clear"))
    (ensure-same t (empty-p map)
                 :report (report-name map-class "empty after clear"))))

(defun test-map (map-class constructor-function element-function de-e)
  (test-map-put-objects map-class constructor-function element-function de-e))
