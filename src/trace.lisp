;;;; trace.lisp --- trace class provided by the model.transform.trace system.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:model.transform.trace)

(defclass trace (print-items:print-items-mixin)
  ((transform :initarg  :transform
              :reader   transform
              :initform nil)
   (sources   :initarg  :sources
              :type     list
              :reader   sources
              :initform '())
   (targets   :initarg  :targets
              :type     list
              :reader   targets
              :initform '())))

(defmethod print-items:print-items append ((object trace))
  `((:sources   ,(sources object)   "~:[∅~;~:*~{~A~^ ~}~] -")
    (:transform ,(transform object) "~@[~A~]"                 ((:after :sources)))
    (:targets   ,(targets object)   "-> ~:[∅~;~:*~{~A~^ ~}~]" ((:after :transform)))))

(defun make-trace (source-or-sources target-or-targets
                   &optional (transform *transform*))
  (make-instance 'trace
                 :sources   (ensure-list source-or-sources)
                 :targets   (ensure-list target-or-targets)
                 :transform transform))

(defmethod source ((trace trace))
  (let ((sources (sources trace)))
    (assert (length= 1 sources))
    sources))

(defmethod target ((trace trace))
  (let ((targets (targets trace)))
    (assert (length= 1 targets))
    targets))
