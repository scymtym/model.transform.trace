;;;; tracer.lisp --- tracer class provided by the model.transform.trace system.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:model.transform.trace)

(defclass tracer (print-items:print-items-mixin)
  ((traces           :reader   traces
                     :initform (make-array 0 :adjustable t :fill-pointer 0))
   (traces-by-source :reader   %traces-by-source
                     :initform (make-hash-table :test #'eq))
   (traces-by-target :reader   %traces-by-target
                     :initform (make-hash-table :test #'eq))))

(defmethod print-items:print-items append ((object tracer))
  `((:trace-count ,(length (traces object)) "(~:D)")))

(defmethod traces-for-source ((tracer tracer) (source t))
  (gethash source (%traces-by-source tracer)))

(defmethod direct-targets-for-source ((tracer tracer) (source t))
  (mappend #'targets (traces-for-source tracer source)))

(defmethod direct-target-for-source ((tracer tracer) (source t))
  (when-let* ((traces  (traces-for-source tracer source))
              (targets (progn
                         (assert (length= 1 traces))
                         (targets (first traces)))))
    (assert (length= 1 targets))
    (first targets)))

(defmethod trace-for-target ((tracer tracer) (target t))
  (gethash target (%traces-by-target tracer)))

(defmethod direct-sources-for-target ((tracer tracer) (target t))
  (when-let ((trace (trace-for-target tracer target)))
    (sources trace)))

(defmethod direct-source-for-target ((tracer tracer) (target t))
  (when-let ((sources (direct-sources-for-target tracer target)))
    (assert (length= 1 sources))
    (first sources)))

(defmethod add-trace :before ((tracer tracer) (trace t))
  (let ((by-target (%traces-by-target tracer)))
    (map nil (lambda (target)
               (when-let ((existing-trace (gethash target by-target '())))
                 (error 'duplicate-trace-error
                        :tracer         tracer
                        :target         target
                        :existing-trace existing-trace)))
         (targets trace))))

(defmethod add-trace ((tracer tracer) (trace t))
  (let ((by-source (%traces-by-source tracer))
        (by-target (%traces-by-target tracer)))
    (vector-push-extend trace (traces tracer))
    (map nil (lambda (source)
               (push trace (gethash source by-source '())))
         (sources trace))
    (map nil (lambda (target)
               (setf (gethash target by-target) trace))
         (targets trace))))
