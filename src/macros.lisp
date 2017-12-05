;;;; macro.lisp --- Macros provided by the model.transform.trace system.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:model.transform.trace)

;;; Context management

(defmacro with-tracer ((tracer) &body body)
  "Execute BODY with the current tracer bound to TRACER."
  `(let ((*tracer* ,tracer)) ,@body))

(defmacro with-transform ((transform) &body body)
  "Execute BODY with the current transform bound to TRANSFORM."
  `(let ((*transform* ,transform)) ,@body))

;;; {recording,ensured}-transform[*]

(defun make-call (function argument-forms sources-forms body
                  last-source-is-list?)
  (when (and last-source-is-list? (null sources-forms))
    (error "~@<At least one source form is required.~@:>"))
  (if last-source-is-list?
      `(apply #',function (lambda () ,@body) ,@argument-forms
              ,@(butlast sources-forms) ,(lastcar sources-forms))
      `(,function (lambda () ,@body) ,@argument-forms ,@sources-forms)))

(defmacro with-current-sources ((&rest sources) &body body)
  (make-call 'call-with-current-sources '() sources body nil))

(defmacro with-current-sources* ((&rest sources) &body body)
  (make-call 'call-with-current-sources '() sources body t))

(macrolet
    ((define-macros (name function)
       (flet ((one-macro (name last-source-is-list?)
                `(defmacro ,name (((&optional (transform '*transform*)
                                              (tracer    '*tracer*))
                                   &rest sources)
                                  &body body)
                   ,(format nil "Add a trace for TRANSFORM, SOURCES ~
                                 and the result of BODY to TRACER.")
                   (make-call ',function tracer transform sources body
                              ,last-source-is-list?))))
         (let ((name* (symbolicate name '*)))
           `(progn
              ,(one-macro name  nil)
              ,(one-macro name* t))))))

  (define-macros recording-transform call-recording-transform)

  (define-macros ensured-transform   ensure-transformed))
