;;;; protocol.lisp --- Protocol functions provided by the model.transform.trace system.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:model.transform.trace)

;;; Trace protocol

(defgeneric sources (trace)
  (:documentation
   "Return sequence of source objects stored in TRACE."))

(defgeneric targets (trace)
  (:documentation
   "Return sequence of target objects stored in TRACE."))

(defgeneric transform (trace)
  (:documentation
   "Return transform stored in TRACE."))

;;; Tracer protocol

(defun make-tracer ()
  "Return an object suitable for use with the tracer protocol."
  (make-instance 'tracer))

(defgeneric traces (tracer)
  (:documentation
   "Return sequence of traces stored in TRACER."))

(defgeneric traces-for-source (tracer source)
  (:documentation
   "Return all traces in TRACER in which SOURCE is a source object."))

(defgeneric direct-targets-for-source (tracer source)
  (:documentation
   "Return sequence of targets generated from SOURCE in TRACER."))

(defgeneric direct-target-for-source (tracer source)
  (:documentation
   "Return the target generated from SOURCE in TRACER.

    Signal an error if more than one target objects have been
    generated from SOURCE."))

(defgeneric trace-for-target (tracer target)
  (:documentation
   "Return the trace in TRACER in which TARGET is the target object.

    Return nil if such a trace does not exist in TRACER."))

(defgeneric direct-sources-for-target (tracer target)
  (:documentation
   "TODO"))

(defgeneric direct-source-for-target (tracer target)
  (:documentation
   "TODO"))

(defgeneric add-trace (tracer trace)
  (:documentation
   "Add TRACE to TRACER."))

;;; Transitive sources protocol

(defgeneric walk-sources-for-target (function tracer target)
  (:documentation
   "Call FUNCTION for objects from which TARGET is derived in TRACER.

    The lambda list of FUNCTION has to be compatible with

      (recurse source target) ; TODO transform?

    where

      TARGET and SOURCE are target and source objects in a trace in
      TRACER

      and RECURSE is a function that, when called, continues the
      traversal at direct sources of SOURCE.

    For sources reachable via multiple paths from TARGET, FUNCTION is
    called once for each such path."))

(defgeneric sources-for-target (tracer target)
  (:documentation
   "TODO"))

;;; Default behavior

(defmethod walk-sources-for-target ((tracer t) (function t) (target t))
  (walk-sources-for-target tracer (ensure-function function) target))

(defmethod walk-sources-for-target ((tracer t) (function function) (target t))
  (labels ((rec (function target)
             (when-let ((trace (trace-for-target tracer target)))
               (let ((transform (transform trace)))
                 (map 'list (lambda (source)
                              (labels ((recurse (&key (function function))
                                         (rec function source)))
                                (funcall function #'recurse transform source target)))
                      (sources trace))))))
    (rec function target)))

(defmethod walk-unique-sources-for-target ((tracer t) (function t) (target t))
  (walk-unique-sources-for-target tracer (ensure-function function) target))

(defmethod walk-unique-sources-for-target
    ((tracer t) (function function) (target t))
  (let ((seen (make-hash-table :test #'equal)))
    (labels
        ((visit (function recurse transform source target)
           (let ((key (cons source target)))
             (multiple-value-bind (result seen?) (gethash key seen)
               (when seen?
                 (return-from visit result))
               (setf (gethash key seen)
                     t
                     (gethash key seen)
                     (labels ((recurse (&key (function function))
                                (funcall recurse
                                         :function (curry #'visit function))))
                       (funcall function #'recurse transform source target)))))))
      (walk-sources-for-target tracer (curry #'visit function) target))))

(defmethod sources-for-target ((tracer t) (target t))
  (let ((result '()))
    (walk-unique-sources-for-target tracer
                                    (lambda (recurse target source)
                                      (declare (ignore target))
                                      (unless (funcall recurse)
                                        (push source result))
                                      t)
                                    target)
    result))

(defmethod map-roots-for-target ((tracer t) (function t) (target t))
  (map-roots-for-target (ensure-function function) tracer target))

(defmethod map-roots-for-target ((tracer t) (function function) (target t))
  (flet ((visit (recurse transform source target)
           (declare (ignore transform target))
           (unless (funcall recurse)
             (funcall function source))
           t))
    (walk-unique-sources-for-target tracer #'visit target)))

(defmethod roots-for-target ((tracer t) (target t))
  (let ((result '()))
    (map-roots-for-target tracer
                          (lambda (root)
                            (push root result))
                          target)
    result))

;;; Abbreviated versions

(macrolet ((define-abbreviation (name (&rest args))
             (let* ((name* (symbolicate name '#:*))
                    (&rest (position '&rest args))
                    (args1 (subseq args 0 &rest))
                    (rest  (when &rest (nth (1+ &rest) args))))
               `(defun ,name* ,args
                  ,(format nil "Like `~(~A~)' but uses `*tracer*' ~
                                instead of accepting a tracer ~
                                parameter."
                           name)
                  ,(if rest
                       `(apply #',name *tracer* ,@args1 ,rest)
                       `(,name *tracer* ,@args1))))))
  (define-abbreviation traces ())

  (define-abbreviation traces-for-source (source))
  (define-abbreviation direct-targets-for-source (source))
  (define-abbreviation direct-target-for-source (source))

  (define-abbreviation trace-for-target (target))
  (define-abbreviation direct-sources-for-target (target))
  (define-abbreviation direct-source-for-target (target))

  (define-abbreviation add-trace (trace))

  (define-abbreviation walk-sources-for-target (function target))
  (define-abbreviation walk-unique-sources-for-target (function target))
  (define-abbreviation sources-for-target (target))
  (define-abbreviation map-roots-for-target (function target))
  (define-abbreviation roots-for-target (target)))
