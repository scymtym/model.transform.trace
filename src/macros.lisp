;;;; macro.lisp --- Macros provided by the model.transform.trace system.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:model.transform.trace)

(defmacro recording-transform ((tracer transform &rest sources) &body body)
  `(call-recording-transform (lambda () ,@body) ,tracer ,transform ,@sources))

(defmacro ensured-transform ((tracer transform &rest sources) &body body)
  `(ensure-transformed (lambda () ,@body) ,tracer ,transform ,@sources))
