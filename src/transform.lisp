;;;; transform.lisp --- transform-related functions provided by the model.transform.trace system.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:model.transform.trace)

(defun call-recording-transform (thunk tracer transform &rest sources)
  (let ((targets (multiple-value-list (funcall thunk))))
    (add-trace tracer (make-trace sources targets transform))
    (values-list targets)))

(defun ensure-transformed (thunk tracer transform &rest sources)
  (assert (not (null sources)))
  (if-let ((cached-targets
            (if (length= 1 sources)
                (direct-targets-for-source
                 tracer (first sources)) ; TODO support (eq target nil) ?
                (block nil
                  (map nil (lambda (trace)
                             (when (set-equal (sources trace) sources
                                              :test #'eq)
                               (return (targets trace))))
                       (traces-for-source tracer (first sources)))))))
    (values-list cached-targets)
    (apply #'call-recording-transform thunk tracer transform sources)))
