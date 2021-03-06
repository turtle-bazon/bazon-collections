;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections-tests)

(defun test-collection-empty-at-begin (collection-class collection)
  (ensure-same 0 (size collection)
	       :report (report-name collection-class "size 0"))
  (ensure-same t (empty-p collection)
	       :report (report-name collection-class "empty at begin")))

(defun test-collection-add-object (collection-class collection element-function de-e)
  (add-object collection (funcall element-function 1))
  (ensure-same 1 (funcall de-e
			  (let ((iterator (c-it (iterator collection))))
			    (it-next iterator)
			    (it-current iterator)))
	       :report (report-name collection-class "1 is single element after add 1"))
  (ensure-same 1 (size collection)
	       :report (report-name collection-class "size 1 after add 1"))
  (ensure-same nil (empty-p collection)
	       :report (report-name collection-class "non empty after add 1")))

(defun test-collection-contains (collection-class collection element-function de-e)
  (ensure-same t (contains collection (funcall element-function 1))
	       :report (report-name collection-class "contains added 1"))
  (ensure-same 1 (funcall de-e (it-current (find-object collection (funcall element-function 1))))
	       :report (report-name collection-class "1 is that one found by iterator")))

(defun test-collection-clear (collection-class collection)
  (clear collection)
  (ensure-same 0 (size collection)
	       :report (report-name collection-class "size 0 after clear"))
  (ensure-same t (empty-p collection)
	       :report (report-name collection-class "empty after clear")))

(defun test-collection-find-object (collection-class collection element-function de-e)
  (add-object collection (funcall element-function 0))
  (add-object collection (funcall element-function 1))
  (ensure-same 1 (funcall de-e (it-current (find-object collection (funcall element-function 1))))
	       :report (report-name collection-class "1 is that one found by iterator"))
  (ensure-same 2 (size collection)
	       :report (report-name collection-class "size 2 after add 0, 1"))
  (ensure-same nil (empty-p collection)
	       :report (report-name collection-class "non empty after add 0, 1")))

(defun test-collection-remove-object (collection-class collection element-function de-e)
  (remove-object collection (funcall element-function 0))
  (ensure-same 1 (funcall de-e
			  (let ((iterator (c-it (iterator collection))))
			    (it-next iterator)
			    (it-current iterator)))
	       :report (report-name collection-class "1 is single element after removing 0"))
  (ensure-same 1 (size collection)
	       :report (report-name collection-class "size 1 after remove 0"))
  (ensure-same nil (empty-p collection)
	       :report (report-name collection-class "non empty after remove 0"))
  (ensure-same nil (find-object collection (funcall element-function 0))
	       :report (report-name collection-class "0 not found after removing it")))

(defun test-collection-add-all-objects (collection-class collection element-function de-e)
  (add-all-objects collection (mapcar element-function '(2 3 4)))
  (let ((iterator (c-it (iterator collection))))
    (ensure-same 10 (loop while (it-next iterator)
			  sum (funcall de-e (it-current iterator)))
		 :report (report-name collection-class "10 sum of elements after adding 2, 3, 4")))
  (ensure-same 4 (size collection)
	       :report (report-name collection-class "size 4 after add 2, 3, 4"))
  (ensure-same nil (empty-p collection)
	       :report (report-name collection-class "non empty after add 2, 3, 4")))

(defun test-collection-remove-all-objects (collection-class collection element-function de-e)
  (remove-all-objects collection (mapcar element-function '(1 2 3)))
  (ensure-same 4 (funcall de-e
			  (let ((iterator (c-it (iterator collection))))
			    (it-next iterator)
			    (it-current iterator)))
	       :report (report-name collection-class "4 is single element after removing 1, 2, 3"))
  (ensure-same 1 (size collection)
	       :report (report-name collection-class "size 1 after remove 1, 2, 3"))
  (ensure-same nil (empty-p collection)
	       :report (report-name collection-class "non empty after remove 1, 2, 3")))

(defun test-collection-add-all-objects-remove-all-objects-c (collection-class collection constructor-function element-function de-e)
  (let ((add-collection (funcall constructor-function))
	(remove-collection (funcall constructor-function)))
    (add-all-objects add-collection (mapcar element-function '(5 6 7)))
    (add-all-objects remove-collection (mapcar element-function '(4 5 6 7)))
    (add-all-objects collection add-collection)
    (let ((iterator (c-it (iterator collection))))
      (ensure-same 22 (loop while (it-next iterator)
			    sum (funcall de-e (it-current iterator)))
		   :report (report-name collection-class "2 sum of elements after adding 5, 6, 7")))
    (ensure-same 4 (size collection)
		 :report (report-name collection-class "size 4 after add 5, 6, 7 (C)"))
    (ensure-same nil (empty-p collection)
		 :report (report-name collection-class "non empty after add 5, 6, 7 (C)"))
    (remove-all-objects collection remove-collection)
    (ensure-same 0 (size collection)
		 :report (report-name collection-class "size 0 after remove 4, 5, 6, 7 (C)"))
    (ensure-same t (empty-p collection)
		 :report (report-name collection-class "empty after remove 4, 5, 6, 7 (C)"))))

(defun test-collection-simple-iterator (collection-class collection de-e)
  (let ((simple-iterator (iterator collection)))
    (ensure-same 15 (loop while (it-next simple-iterator)
			  sum (funcall de-e (it-current simple-iterator)))
		 :report (report-name collection-class "simple iterator count"))))

(defun test-collection-conditional-iterator (collection-class collection de-e)
  (let ((conditional-iterator (iterator collection
					(lambda (number)
					  (evenp (funcall de-e number))))))
    (ensure-same 6 (loop while (it-next conditional-iterator)
			 sum (funcall de-e (it-current conditional-iterator)))
		 :report (report-name collection-class "conditional iterator count"))))

(defun test-collection-remove-with-iterator (collection-class collection de-e)
  (let ((simple-iterator (iterator collection)))
    (it-next simple-iterator)
    (let ((removed-element (funcall de-e (it-current simple-iterator))))
      (remove-object collection simple-iterator)
      (ensure-same (- 15 removed-element)
		   (loop while (it-next simple-iterator)
			 sum (funcall de-e (it-current simple-iterator)))
		   :report (report-name collection-class "simple iterator count after remove")))
    (ensure-same 4 (size collection)
		 :report (report-name collection-class "size 4 after remove with iterator"))
    (ensure-same nil (empty-p collection)
		 :report (report-name collection-class "non empty after remove with iterator"))))

(defun test-collection-remove-all-with-iterator (collection-class collection de-e)
  (let ((conditional-iterator (c-it (iterator collection
					      (lambda (number)
						(oddp (funcall de-e number)))))))
    (remove-all-objects collection conditional-iterator)
    (let ((iterator (c-it (iterator collection))))
      (ensure-same 6 (loop while (it-next iterator)
			    sum (funcall de-e (it-current iterator)))
		   :report (report-name collection-class "6 sum of element after removing all odd with iterator")))
    (ensure-same 2 (size collection)
		 :report (report-name collection-class "size 2 after remove all in iterator odd numbers"))
    (ensure-same nil (empty-p collection)
		 :report (report-name collection-class "non empty after remove in iterator odd numbers"))))

(defun test-collection-find-all-objects (collection-class collection de-e)
  (let ((all-found-iterator (find-all-objects collection
					      (lambda (number)
						(oddp (funcall de-e number))))))
    (ensure-same 9 (loop while (it-next all-found-iterator)
		      sum (funcall de-e (it-current all-found-iterator)))
		 :report (report-name collection-class "find-all-objects iterator sum for odd is 9"))))

(defun test-collection (collection-class
			constructor-function element-function de-e)
  (let ((collection (funcall constructor-function)))
    (test-collection-empty-at-begin collection-class collection)
    (test-collection-add-object collection-class collection element-function de-e) ; (1)
    (test-collection-contains collection-class collection element-function de-e) ; (1)
    (test-collection-clear collection-class collection) ; ()
    (test-collection-find-object collection-class collection element-function de-e) ; (0 1)
    (test-collection-remove-object collection-class collection element-function de-e) ; (1)
    (test-collection-add-all-objects collection-class collection element-function de-e) ; (1 2 3 4)
    (test-collection-remove-all-objects collection-class collection element-function de-e) ; (4)
    (test-collection-add-all-objects-remove-all-objects-c collection-class collection constructor-function element-function de-e) ; ()
    (add-all-objects collection (mapcar element-function '(1 2 3 4 5))) ; (1 2 3 4 5)
    (test-collection-simple-iterator collection-class collection de-e) ; (1 2 3 4 5)
    (test-collection-conditional-iterator collection-class collection de-e) ; (1 2 3 4 5)
    (test-collection-remove-with-iterator collection-class collection de-e) ; (1 2 3 4 5) - undefined
    )
  (let ((collection (funcall constructor-function)))
    (add-all-objects collection (mapcar element-function '(1 2 3 4 5))) ; (1 2 3 4 5)
    (test-collection-remove-all-with-iterator collection-class collection de-e) ; (2 6)
    )
  (let ((collection (funcall constructor-function)))
    (add-all-objects collection (mapcar element-function '(1 2 3 4 5)))
    (test-collection-find-all-objects collection-class collection de-e) ; (1 2 3 4 5)
    ))
