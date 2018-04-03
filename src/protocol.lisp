;;;; protocol.lisp --- Protocol functions provided by the model.transform.trace system.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
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

(defgeneric traces-for-target (tracer target)
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

;;;

(macrolet
    ((define-transform-methods (query-object-name)
       (let* ((name            (symbolicate query-object-name
                                            '#:s-for-transform))
              (map-name        (symbolicate '#:map- name))
              (direct-accessor (symbolicate query-object-name '#:s)))
         `(progn
            (defgeneric ,map-name (tracer function transform))

            (defgeneric ,name (tracer transform))

            (defmethod ,map-name ((tracer t) (function t) (transform t))
              (,map-name tracer (ensure-function function) transform))

            (defmethod ,map-name ((tracer t) (function function) (transform t))
              (mapcan (lambda (trace)
                        (map 'list function (,direct-accessor trace)))
                      (traces-for-transform tracer transform)))

            (defmethod ,name ((tracer t) (transform t))
              (let ((result '()))
                (,map-name tracer
                           (lambda (,query-object-name)
                             (push ,query-object-name result))
                           transform)
                result))))))
  (define-transform-methods source)
  (define-transform-methods target))

;;; Transitive sources and targets protocol

(macrolet
    ((define-transitivity-protocol (query-object-name opposite-object-name terminal-object-name)
       (let* ((traces-for-name      (symbolicate '#:traces-for- query-object-name))
              (opposite-reader-name (symbolicate opposite-object-name '#:s))
              (name                 (symbolicate opposite-object-name
                                                 '#:s-for- query-object-name))
              (walk-name            (symbolicate '#:walk- name))
              (walk-unique-name     (symbolicate '#:walk-unique- name))
              (terminal-name        (symbolicate terminal-object-name
                                                 '#:s-for- query-object-name))
              (map-terminal-name    (symbolicate '#:map- terminal-name)))
         `(progn
            (defgeneric ,walk-name (tracer function ,query-object-name)
              (:documentation
               "Call FUNCTION for objects from which TARGET is derived in TRACER.

                The lambda list of FUNCTION has to be compatible with

                  (recurse transform source target)

                where

                  TARGET and SOURCE are target and source objects in a trace in
                  TRACER

                  and RECURSE is a function that, when called, continues the
                  traversal at direct sources of SOURCE.

                For sources reachable via multiple paths from TARGET, FUNCTION is
                called once for each such path."))

            (defgeneric ,name (tracer ,query-object-name)
              (:documentation
               "TODO"))

            ;;; Default behavior

            (defmethod ,walk-name ((tracer t) (function t) (,query-object-name t))
              (,walk-name tracer (ensure-function function) ,query-object-name))

            (defmethod ,walk-name ((tracer t) (function function) (,query-object-name t))
              (labels ((rec (function ,query-object-name)
                         (mapcan (lambda (trace)
                                   (let ((transform (transform trace)))
                                     (map 'list (lambda (,opposite-object-name)
                                                  (labels ((recurse (&key (function function))
                                                             (rec function ,opposite-object-name)))
                                                    (funcall function #'recurse transform source target)))
                                          (,opposite-reader-name trace))))
                                 (,traces-for-name tracer ,query-object-name))))
                (rec function ,query-object-name)))

            (defmethod ,walk-unique-name
                ((tracer t) (function t) (,query-object-name t))
              (,walk-unique-name tracer (ensure-function function) ,query-object-name))

            (defmethod ,walk-unique-name
                ((tracer t) (function function) (,query-object-name t))
              (let ((seen (make-hash-table :test #'equal)))
                (labels
                    ((visit (function recurse transform source target)
                       (let ((key (cons ,opposite-object-name ,query-object-name))) ; TODO do uniqe traces instead?
                         (multiple-value-bind (result seen?) (gethash key seen)
                           (when seen?
                             (return-from visit result))
                           (setf (gethash key seen)
                                 ;; Changing this to nil so `leafs-for-source' does not fail for cases like
                                 ;; A -- transform1 --> B -- transform2 --+
                                 ;;                     ^                 |
                                 ;;                     +-----------------+
                                 ;; let's see what breaks
                                 nil ; was t
                                 (gethash key seen)
                                 (labels ((recurse (&key (function function))
                                            (funcall recurse
                                                     :function (curry #'visit function))))
                                   (funcall function #'recurse transform source target)))))))
                  (,walk-name tracer (curry #'visit function) ,query-object-name))))

            (defmethod ,name ((tracer t) (,query-object-name t))
              (let ((result '()))
                (,walk-unique-name
                 tracer
                 (lambda (recurse transform source target)
                   (declare (ignore transform ,query-object-name))
                   (push ,opposite-object-name result)
                   (funcall recurse)
                   #+previous (unless (funcall recurse)
                     (push ,opposite-object-name result))
                   #+previous t)
                 ,query-object-name)
                result))

            (defmethod ,map-terminal-name ((tracer t) (function t) (,query-object-name t))
              (,map-terminal-name (ensure-function function) tracer ,query-object-name))

            (defmethod ,map-terminal-name ((tracer t) (function function) (,query-object-name t))
              (flet ((visit (recurse transform source target)
                       (declare (ignore transform ,query-object-name))
                       (unless (find t (funcall recurse))
                         (funcall function ,opposite-object-name)
                         t)))
                (,walk-unique-name tracer #'visit ,query-object-name)))

            (defmethod ,terminal-name ((tracer t) (,query-object-name t))
              (let ((result '()))
                (,map-terminal-name tracer
                                    (lambda (,terminal-object-name)
                                      (push ,terminal-object-name result))
                                    ,query-object-name)
                result))))))

  (define-transitivity-protocol target source root)
  (define-transitivity-protocol source target leaf))

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

  (define-abbreviation traces-for-transform (transform))

  (define-abbreviation traces-for-source (source))
  (define-abbreviation direct-targets-for-source (source))
  (define-abbreviation direct-target-for-source (source))

  (define-abbreviation traces-for-target (target))
  (define-abbreviation direct-sources-for-target (target))
  (define-abbreviation direct-source-for-target (target))

  (define-abbreviation add-trace (trace))

  (define-abbreviation map-sources-for-transform (function transform))
  (define-abbreviation sources-for-transform (transform))
  (define-abbreviation map-targets-for-transform (function transform))
  (define-abbreviation targets-for-transform (transform))

  (define-abbreviation walk-sources-for-target (function target))
  (define-abbreviation walk-unique-sources-for-target (function target))
  (define-abbreviation sources-for-target (target))
  (define-abbreviation map-roots-for-target (function target))
  (define-abbreviation roots-for-target (target))

  (define-abbreviation walk-targets-for-source (function source))
  (define-abbreviation walk-unique-targets-for-source (function source))
  (define-abbreviation targets-for-source (source))
  (define-abbreviation map-leafs-for-source (function source))
  (define-abbreviation leafs-for-source (source)))

;;; Recording transforms
