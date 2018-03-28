;;;; package.lisp --- Package definition for the model.transform.trace system.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:model.transform.trace
  (:use
   #:cl
   #:alexandria)

  (:shadow
   #:trace)

  ;; Variables
  (:export
   #:*tracer*)

  ;; Trace protocol
  (:export
   #:sources
   #:targets
   #:transform

   #:make-trace)

  ;; Tracer protocol
  (:export
   #:make-tracer

   #:traces                    #:traces*

   #:traces-for-transform

   #:traces-for-source         #:traces-for-source*
   #:direct-targets-for-source #:direct-targets-for-source*
   #:direct-target-for-source  #:direct-target-for-source*

   #:trace-for-target          #:trace-for-target*
   #:direct-sources-for-target #:direct-sources-for-target*
   #:direct-source-for-target  #:direct-source-for-target*

   #:add-trace                 #:add-trace*)

  ;;
  (:export
   #:map-sources-for-transform #:map-sources-for-transform*
   #:sources-for-transform     #:sources-for-transform*

   #:map-targets-for-transform #:map-targets-for-transform*
   #:targets-for-transform     #:targets-for-transform*)

  ;; Transitive sources and targets protocol
  (:export
   #:walk-sources-for-target        #:walk-sources-for-target*
   #:walk-unique-sources-for-target #:walk-unique-sources-for-target*
   #:sources-for-target             #:sources-for-target*

   #:map-roots-for-target           #:map-roots-for-target*
   #:roots-for-target               #:roots-for-target*

   #:walk-targets-for-source        #:walk-targets-for-source*
   #:walk-unique-targets-for-source #:walk-unique-targets-for-source*
   #:targets-for-source             #:targets-for-source*

   #:map-leafs-for-source           #:map-leafs-for-source*
   #:leafs-for-source               #:leafs-for-source*)

  ;; Tracer class
  (:export
   #:tracer)

  ;; Transform protocol
  (:export
   #:call-recording-transform #:call-recording-transform*
   #:ensure-transformed       #:ensure-transformed*)

  ;; Macros
  (:export
   #:with-tracer

   #:with-transform

   #:call-with-current-sources #:with-current-sources #:with-current-sources*

   #:call-recording-transform  #:recording-transform  #:recording-transform*

   #:ensure-transformed        #:ensured-transform    #:ensured-transform*)

  (:documentation
   "TODO"))
