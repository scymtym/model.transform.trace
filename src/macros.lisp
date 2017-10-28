;;;; macro.lisp --- Macros provided by the model.transform.trace system.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:model.transform.trace)

;;;

(defmacro with-tracer ((tracer) &body body)
  `(let ((*tracer* ,tracer)) ,@body))

(defmacro with-transform ((transform) &body body)
  `(let ((*transform* ,transform)) ,@body))

;;; {recording,ensured}-transform[*]

(defun make-call (function
                  tracer-form transform-form sources-forms body
                  last-source-is-list?)
  (when (and last-source-is-list? (null sources-forms))
    (error "~@<At least one source form is required.~@:>"))
  `(,function (lambda () ,@body) ,tracer-form ,transform-form
              ,@(if last-source-is-list?
                    sources-forms
                    `(,@(butlast sources-forms)
                        ,(lastcar sources-forms)))))

(macrolet
    ((define-macros (name function)
       (flet ((one-macro (name last-source-is-list?)
                `(defmacro ,name (((&optional (transform '*transform*)
                                              (tracer    '*tracer*))
                                   &rest sources)
                                  &body body)
                   (make-call ',function tracer transform sources body
                              ,last-source-is-list?))))
         (let ((name* (symbolicate name '*)))
           `(progn
              ,(one-macro name  nil)
              ,(one-macro name* t))))))

  (define-macros recording-transform call-recording-transform)

  (define-macros ensured-transform   ensure-transformed))
