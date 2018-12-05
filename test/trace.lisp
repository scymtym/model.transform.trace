;;;; trace.lisp --- Test for the trace class and associated functions.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:model.transform.trace.test)

(def-suite* :model.transform.trace.trace
  :in :model.transform.trace)

(test trace.print
  "Test printing `trace' instances."

  (is (not (emptyp (princ-to-string (make-trace 1 2 nil))))))
