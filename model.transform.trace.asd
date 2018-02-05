(defsystem "model.transform.trace"

  :depends-on ("alexandria"
               "utilities.print-items")

  :components ((:module     "trace"
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

  :depends-on ("fiveam"

               "model.transform.trace")

  :components ((:module     "trace"
                :pathname   "test"
                :serial     t
                :components ((:file       "package"))))

  :perform (test-op (operation component)
             (uiop:symbol-call '#:model.transform.trace.test '#:run-tests)))
