;;; -*- lisp -*-

(defsystem :bazon-collections
  :name "bazon-collections"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "Lessor Lisp General Public License"
  :version "0.0.1.4"
  :description "Common Lisp Collections library"
  :depends-on (bordeaux-threads)
  :components ((:module src
		:components
		((:file "package")
		 (:module api
		  :depends-on ("package")
		  :components
		  ((:file "iterator")
		   (:file "collection"
		    :depends-on ("iterator"))
		   (:file "list"
		    :depends-on ("collection"))
		   (:file "linear-list"
		    :depends-on ("collection"))
		   (:file "queue"
		    :depends-on ("linear-list"))
		   (:file "stack"
		    :depends-on ("linear-list"))
		   (:file "set"
		    :depends-on ("collection"))
		   (:file "map"
		    :depends-on ("collection"))
		   (:file "tree"
		    :depends-on ("collection"))))
		 (:module impl
		  :depends-on ("package"
			       "api")
		  :components
		  ((:file "array-list")
		   (:file "linked-list")
		   (:file "simple-queue")
		   (:file "graham-queue")
                   (:file "blocking-queue")
		   (:file "simple-stack")
		   (:file "cons-stack")
		   (:file "cons-set")
                   (:file "hash-map")
                   (:file "hash-set"
                    :depends-on ("hash-map")))))))
  :in-order-to ((test-op (test-op bazon-collections-tests)))
  :perform (test-op :after (op c)
		    (funcall
		     (intern (symbol-name '#:run-all-tests)
			     :bazon-collections-tests))))
