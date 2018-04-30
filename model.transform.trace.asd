(defsystem "model.transform.trace"
  :description "Facilities for tracking inputs and outputs of model transformations"
  :license     "LLGPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "utilities.print-items" "0.1"))

  :components  ((:module     "trace"
                 :pathname   "src"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "variables")
                              (:file       "protocol")
                              (:file       "conditions")

                              (:file       "trace")
                              (:file       "tracer")

                              (:file       "transform")

                              (:file       "macros"))))

  :in-order-to ((test-op (test-op "model.transform.trace/test"))))

(defsystem "model.transform.trace/test"
  :description "Tests for the model.transform.trace system"
  :license     "LLGPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "fiveam"                "1.4")

                (:version "model.transform.trace" (:read-file-form "version-string.sexp")))

  :components  ((:module     "trace"
                 :pathname   "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")

                              (:file       "trace")
                              (:file       "tracer"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:model.transform.trace.test '#:run-tests)))
