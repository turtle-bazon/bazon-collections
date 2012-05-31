;;; -*- lisp -*-

(defsystem :bazon-collections-tests
  :name "bazon-collections-tests"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "Lessor Lisp General Public License"
  :version "0.0.1.0"
  :description "Common Lisp Collections framework tests"
  :depends-on (:bazon-collections :lift :iterate)
  :components ((:module tests-collections
                        :components
			((:file "package")
			 (:file "bazon-collections-tests" :depends-on ("package"))))))
