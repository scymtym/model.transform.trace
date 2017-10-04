;;;; macro.lisp --- Macros provided by the model.transform.trace system.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:model.transform.trace)

;;;

(defmacro with-tracer ((tracer) &body body)
  `(let ((*tracer* ,tracer)) ,@body))

;;;

(defmacro recording-transform ((tracer transform &rest sources) &body body)
  `(call-recording-transform (lambda () ,@body) ,tracer ,transform ,@sources))

(defmacro recording-transform* ((transform &rest sources) &body body)
  `(call-recording-transform (lambda () ,@body) *tracer* ,transform ,@sources))

(defmacro ensured-transform ((tracer transform &rest sources) &body body)
  `(ensure-transformed (lambda () ,@body) ,tracer ,transform ,@sources))

(defmacro ensured-transform* ((transform &rest sources) &body body)
  `(ensure-transformed (lambda () ,@body) *tracer* ,transform ,@sources))

(defmacro recording-transform/list ((tracer transform sources) &body body)
  `(apply #'call-recording-transform (lambda () ,@body) ,tracer ,transform ,sources))

(defmacro recording-transform/list* ((transform sources) &body body)
  `(apply #'call-recording-transform (lambda () ,@body) *tracer* ,transform ,sources))

(defmacro ensured-transform/list ((tracer transform sources) &body body)
  `(apply #'ensure-transformed (lambda () ,@body) ,tracer ,transform ,sources))

(defmacro ensured-transform/list* ((transform sources) &body body)
  `(apply #'ensure-transformed (lambda () ,@body) *tracer* ,transform ,sources))
