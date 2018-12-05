;;;; tracer.lisp --- tracer class provided by the model.transform.trace system.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:model.transform.trace)

(defclass tracer (print-items:print-items-mixin) ; TODO rename to standard-tracer
  ((traces              :reader   traces
                        :initform (make-array 0 :adjustable t :fill-pointer 0))
   (traces-by-transform :reader   %traces-by-transform
                        :initform (make-hash-table :test #'eq))
   (traces-by-source    :reader   %traces-by-source
                        :initform (make-hash-table :test #'eq))
   (traces-by-target    :reader   %traces-by-target
                        :initform (make-hash-table :test #'eq))))

(defmethod print-items:print-items append ((object tracer))
  `((:trace-count ,(length (traces object)) "~:D trace~:P")))

;;; Tracer protocol methods

(defmethod traces-for-transform ((tracer tracer) (transform t))
  (gethash transform (%traces-by-transform tracer)))

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

(defmethod traces-for-target ((tracer tracer) (target t))
  (gethash target (%traces-by-target tracer)))

(defmethod direct-sources-for-target ((tracer tracer) (target t))
  (mappend #'sources (traces-for-target tracer target)))

(defmethod direct-source-for-target ((tracer tracer) (target t))
  (when-let* ((traces  (traces-for-target tracer target))
              (sources (progn
                         (assert (length= 1 traces))
                         (sources (first traces)))))
    (assert (length= 1 sources))
    (first sources)))

(defmethod clear! ((tracer tracer))
  (setf (fill-pointer (traces tracer)) 0)
  (clrhash (%traces-by-transform tracer))
  (clrhash (%traces-by-source tracer))
  (clrhash (%traces-by-target tracer)))

(defmethod add-trace :before ((tracer tracer) (trace t))
  ;; This can legitimately happen if e.g. transform T1 produces an
  ;; object O as its output and a second transform T2 is the identity
  ;; on O.
  #+no (let ((by-target (%traces-by-target tracer)))
         (map nil (lambda (target)
                    (when-let ((existing-trace (gethash target by-target '())))
                      (error 'duplicate-trace-error
                             :tracer         tracer
                             :target         target
                             :existing-trace existing-trace)))
              (targets trace)))

  #+no (let ((by-target (%traces-by-target tracer)))
    (map nil (lambda (target)
               (unless (or (typep target '(or symbol number))
                           (find target (sources trace) :test #'eq))
                 #+no (when-let ((existing-trace (gethash target by-target '())))
                   (error 'duplicate-trace-error
                          :tracer         tracer
                          :target         target
                          :existing-trace existing-trace))))
         (targets trace))))

(defmethod add-trace ((tracer tracer) (trace t))
  (let ((by-transform (%traces-by-transform tracer))
        (by-source    (%traces-by-source tracer))
        (by-target    (%traces-by-target tracer)))
    (vector-push-extend trace (traces tracer))
    (push trace (gethash (transform trace) by-transform '()))
    (map nil (lambda (source)
               (push trace (gethash source by-source '())))
         (sources trace))
    (map nil (lambda (target)
               (progn ; unless (typep target '(or symbol number)) ; TODO just push here as well?
                 (push trace (gethash target by-target '()))))
         (targets trace))))
