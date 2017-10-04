(defsystem :model.transform.trace

  :depends-on (:utilities.print-items)

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

                             (:file       "macros")))))
