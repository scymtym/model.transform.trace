;;;; conditions.lisp --- Conditions signaled by the model.transform.trace system.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:model.transform.trace)

(define-condition duplicate-trace-error (error)
  ((tracer         :initarg :tracer
                   :reader  tracer)
   (target         :initarg :target
                   :reader  target)
   (existing-trace :initarg :existing-trace
                   :reader  existing-trace))
  #+later (:default-initargs
   :tracer         (missing-required-initarg 'duplicate-trace-error :tracer)
   :target         (missing-required-initarg 'duplicate-trace-error :target)
   :existing-trace (missing-required-initarg 'duplicate-trace-error :existing-trace))
  (:report
   (lambda (condition stream)
     (format stream "~@<~A is already associated with trace ~A in ~A.~@:>"
             (target condition) (existing-trace condition) (tracer condition)))))
