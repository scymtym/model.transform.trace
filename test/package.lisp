;;;; package.lisp --- Package for tests of the model.transform.trace system.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:model.transform.trace.test
  (:use
   #:cl
   #:alexandria

   #:fiveam)

  (:use
   #:model.transform.trace)

  (:export
   #:run-tests)

  (:documentation
   "Tests of the model.transform.trace system."))

(cl:in-package #:model.transform.trace.test)

(def-suite :model.transform.trace
  :description
  "Root test suite for the model.transform.trace system.")

(defun run-tests ()
  (run! :model.transform.trace))
