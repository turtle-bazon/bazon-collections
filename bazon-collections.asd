;;; -*- lisp -*-

(defsystem :bazon-collections
  :name "bazon-collections"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "Lessor Lisp General Public License"
  :version "0.0.1.0"
  :description "Common Lisp Collections framework"
  :depends-on ()
  :components ((:module src
                        :components
			((:file "package")
			 (:file "iterator" :depends-on ("package"))
			 (:file "collection" :depends-on ("package"))
			 (:file "list" :depends-on ("collection"))
			 (:file "array-list" :depends-on ("list"))
			 (:file "linear-list" :depends-on ("collection"))
			 (:file "queue" :depends-on ("linear-list"))
			 (:file "stack" :depends-on ("linear-list"))
			 (:file "set" :depends-on ("collection"))
			 (:file "map" :depends-on ("collection"))
			 (:file "tree" :depends-on ("collection")))))
  :in-order-to ((test-op (test-op bazon-collections-tests)))
  :perform (test-op :after (op c)
		    (funcall
		     (intern (symbol-name '#:run-all-tests)
			     :bazon-collections-tests))))
