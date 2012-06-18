;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections-tests)

(defun test-collection (collection-class
			constructor-function element-function de-e)
  (let ((collection (funcall constructor-function)))
    (ensure-same 0 (size collection)
		 :report (report-name collection-class "size 0"))
    (ensure-same t (empty-p collection)
		 :report (report-name collection-class "empty at begin"))
    (add-object collection (funcall element-function 0))
    (ensure-same 1 (size collection)
		 :report (report-name collection-class "size 1 after add 0"))
    (ensure-same nil (empty-p collection)
		 :report (report-name collection-class "non empty after add 0"))
    (ensure-same t (contains collection (funcall element-function 0))
		 :report (report-name collection-class "contains added 0"))
    (ensure-same 0 (funcall de-e (it-current (find-object collection (funcall element-function 0))))
		 :report (report-name collection-class "0 is that one found by iterator"))
    (clear collection)
    (ensure-same 0 (size collection)
		 :report (report-name collection-class "size 0 after clear"))
    (ensure-same t (empty-p collection)
		 :report (report-name collection-class "empty after clear"))
    (add-object collection (funcall element-function 0))
    (add-object collection (funcall element-function 1))
    (ensure-same 1 (funcall de-e (it-current (find-object collection (funcall element-function 1))))
		 :report (report-name collection-class "1 is that one found by iterator"))
    (ensure-same 2 (size collection)
		 :report (report-name collection-class "size 2 after add 0, 1"))
    (ensure-same nil (empty-p collection)
		 :report (report-name collection-class "non empty after add 0, 1"))
    (remove-object collection (funcall element-function 0))
    (ensure-same 1 (size collection)
		 :report (report-name collection-class "size 1 after remove 0"))
    (ensure-same nil (empty-p collection)
		 :report (report-name collection-class "non empty after remove 0"))
    (ensure-same nil (find-object collection (funcall element-function 0))
		 :report (report-name collection-class "0 not found after removing it"))
    (add-all-objects collection (mapcar element-function '(2 3 4)))
    (ensure-same 4 (size collection)
		 :report (report-name collection-class "size 4 after add 2, 3, 4"))
    (ensure-same nil (empty-p collection)
		 :report (report-name collection-class "non empty after add 2, 3, 4"))
    (remove-all-objects collection (mapcar element-function '(1 2 3)))
    (ensure-same 1 (size collection)
		 :report (report-name collection-class "size 1 after remove 1, 2, 3"))
    (ensure-same nil (empty-p collection)
		 :report (report-name collection-class "non empty after remove 1, 2, 3"))
    (let ((add-collection (funcall constructor-function))
	  (remove-collection (funcall constructor-function)))
      (add-all-objects add-collection (mapcar element-function '(5 6 7)))
      (add-all-objects remove-collection (mapcar element-function '(4 5 6 7)))
      (add-all-objects collection add-collection)
      (ensure-same 4 (size collection)
		   :report (report-name collection-class "size 4 after add 5, 6, 7 (C)"))
      (ensure-same nil (empty-p collection)
		   :report (report-name collection-class "non empty after add 5, 6, 7 (C)"))
      (remove-all-objects collection remove-collection)
      (ensure-same 0 (size collection)
		   :report (report-name collection-class "size 0 after remove 4, 5, 6, 7 (C)"))
      (ensure-same t (empty-p collection)
		   :report (report-name collection-class "empty after remove 4, 5, 6, 7 (C)")))
    (add-all-objects collection (mapcar element-function '(1 2 3 4 5)))
    (let ((simple-iterator (iterator collection)))
      (ensure-same 15 (loop while (it-next simple-iterator)
			    sum (funcall de-e (it-current simple-iterator)))
		   :report (report-name collection-class "simple iterator count")))
    (let ((conditional-iterator (iterator collection
					  (lambda (number)
					    (evenp (funcall de-e number))))))
      (ensure-same 6 (loop while (it-next conditional-iterator)
			   sum (funcall de-e (it-current conditional-iterator)))
		   :report (report-name collection-class "conditional iterator count")))
    (let ((simple-iterator (iterator collection)))
      (it-next simple-iterator)
      (let ((removed-element (funcall de-e (it-current simple-iterator))))
	(remove-object collection simple-iterator)
	(ensure-same (- 15 removed-element)
		     (loop while (it-next simple-iterator)
			   sum (funcall de-e (it-current simple-iterator)))
		     :report (report-name collection-class "simple iterator count after remove"))))
    (ensure-same 4 (size collection)
		 :report (report-name collection-class "size 4 after remove with iterator"))
    (ensure-same nil (empty-p collection)
		 :report (report-name collection-class "non empty after remove with iterator"))
    (clear collection)
    (add-all-objects collection (mapcar element-function '(1 2 3 4 5)))
    (let ((conditional-iterator (iterator collection
					  (lambda (number)
					    (oddp (funcall de-e number))))))
      (remove-all-objects collection conditional-iterator)
      (ensure-same 2 (size collection)
		   :report (report-name collection-class "size 2 after remove all in iterator odd numbers"))
      (ensure-same nil (empty-p collection)
		   :report (report-name collection-class "non empty after remove in iterator odd numbers")))
    (clear collection)
    (add-all-objects collection (mapcar element-function '(1 2 3 4 5)))
    (let ((all-found-iterator (find-all-objects collection
						(lambda (number)
						  (oddp (funcall de-e number))))))
      (ensure-same 9 (loop while (it-next all-found-iterator)
			   sum (funcall de-e (it-current all-found-iterator)))
		   :report (report-name collection-class "find-all-objects iterator count")))))
