;;;; transform.lisp --- transform-related functions provided by the model.transform.trace system.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:model.transform.trace)

(defgeneric call-with-current-sources (thunk &rest sources)
  (:documentation
   "Call THUNK with

    Note: This is intended for detailed error locations within a transform. E.g.

    (let ((input ...))
      (with-transform (:my-transform)
        (recording-transform (() input)
          ...
          (let ((part (part-of input)))
            (with-current-sources (part)
              (work-on part)))
          ...))) "))

(defmethod call-with-current-sources ((thunk function) &rest sources)
  (let ((*sources* sources))
    (funcall thunk)))

(defgeneric call-recording-transform (thunk tracer transform &rest sources))

(defmethod call-recording-transform :around ((thunk function) (tracer t) (transform t)
                                             &rest sources)
  (apply #'call-with-current-sources (lambda () (call-next-method)) sources))

(defmethod call-recording-transform ((thunk function) (tracer t) (transform t)
                                     &rest sources)
  (let ((targets (multiple-value-list (funcall thunk))))
    (add-trace tracer (make-trace sources targets transform))
    (values-list targets)))

(defgeneric ensure-transformed (thunk tracer transform &rest sources))

(defmethod ensure-transformed :around ((thunk function) (tracer t) (transform t)
                                       &rest sources)
  (apply #'call-with-current-sources (lambda () (call-next-method)) sources))

(defmethod ensure-transformed ((thunk function) (tracer t) (transform t)
                               &rest sources)
  (assert (not (null sources)))
  (if-let ((cached-targets
            (if (length= 1 sources)
                (when-let* ((traces (traces-for-source tracer (first sources)))
                            (trace  (find transform traces :key #'transform)))
                  (targets trace))
                (block nil
                  (map nil (lambda (trace)
                             (when (and (eq (transform trace) transform)
                                        (set-equal (sources trace) sources
                                                   :test #'eq))
                               (return (targets trace))))
                       (traces-for-source tracer (first sources)))))))
    (values-list cached-targets)
    (apply #'call-recording-transform thunk tracer transform sources)))
