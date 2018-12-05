;;;; tracer.lisp --- Test for the tracer class and associated functions.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:model.transform.trace.test)

(def-suite* :model.transform.trace.tracer
  :in :model.transform.trace)

(test tracer.print
  "Test printing `tracer' instances."

  (is (not (emptyp (princ-to-string (make-tracer))))))
