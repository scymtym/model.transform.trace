;;;; package.lisp --- Package definition for the model.transform.trace system.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:model.transform.trace
  (:use
   #:cl
   #:alexandria)

  (:shadow
   #:trace)

  ;; Trace protocol
  (:export
   #:sources
   #:targets
   #:transform

   #:make-trace)

  ;; Tracer protocol
  (:export
   #:traces

   #:traces-for-source
   #:direct-targets-for-source #:direct-target-for-source

   #:trace-for-target
   #:direct-sources-for-target #:direct-source-for-target

   #:add-trace)

  ;;; Transitive sources protocol
  (:export
   #:walk-sources-for-target
   #:walk-unique-sources-for-target
   #:sources-for-target

   #:map-roots-for-target
   #:roots-for-target)

  ;; Tracer class
  (:export
   #:tracer)

  ;; Transform protocol
  (:export
   #:call-recording-transform
   #:ensure-transformed)

  ;; Macros
  (:export
   #:recording-transform
   #:ensured-transform)

  (:documentation
   "TODO"))
