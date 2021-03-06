;;; -*- lisp -*-

(defsystem :bazon-collections-tests
  :name "bazon-collections-tests"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "Lessor Lisp General Public License"
  :version "0.0.1.0"
  :description "Common Lisp Collections library tests"
  :depends-on (:bazon-collections
               :bordeaux-threads
               :fiveam
               :iterate)
  :components ((:module tests
                        :components
			((:file "package")
			 (:file "collection-tests"
				:depends-on ("package"))
			 (:file "list-tests"
				:depends-on ("package"))
			 (:file "queue-tests"
				:depends-on ("package"))
                         (:file "blocking-queue-tests"
                                :depends-on ("package"))
			 (:file "stack-tests"
				:depends-on ("package"))
			 (:file "set-tests"
				:depends-on ("package"))
			 (:file "map-tests"
			       :depends-on ("package"))
			 (:file "bazon-collections-tests"
				:depends-on ("package"
					     "collection-tests"
					     "list-tests"
					     "queue-tests"
					     "stack-tests"
					     "set-tests"
					     "map-tests"))))))
