;;; -*- lisp -*-

(defsystem :bazon-synchronized-collections
  :name "bazon-synchronized-collections"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "Lessor Lisp General Public License"
  :version "0.0.1.0"
  :description "Common Lisp Collections framework (synchronized extension)"
  :depends-on (:bazon-collections :bordeaux-threads)
  :components ((:module src-synchronized-collections
                        :components
			((:file "package")))))
