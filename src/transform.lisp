;;;; transform.lisp --- transform-related functions provided by the model.transform.trace system.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:model.transform.trace)

(defun call-recording-transform (thunk tracer transform &rest sources)
  (let ((target (funcall thunk)))
    (add-trace tracer (make-trace sources target transform))
    target))

(defun ensure-transformed (thunk tracer transform &rest sources)
  (labels ((trace (source)
             (or (find transform (traces-for-source tracer source)
                       :test #'eq :key #'transform)
                 (throw 'no-trace nil)))
           (cached-target ()
             (catch 'no-trace
               (let ((traces (map 'list #'trace sources)))
                 (when (every #'eq traces (rest traces))
                   (target (first traces)))))))
    (or (if (length= 1 sources)
            (direct-target-for-source tracer (first sources)) ; TODO support (eq target nil) ?
            (cached-target))
        (apply #'call-recording-transform thunk tracer transform sources))))
