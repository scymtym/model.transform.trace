;;;; protocol.lisp --- Tests for protocol functions.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:model.transform.trace.test)

(def-suite* :model.transform.trace.protocol
  :in :model.transform.trace)

(test leafs-for-source.smoke
  "Smoke test for the `leafs-for-source' functions."

  (let ((tracer (make-tracer))
        (a      (list nil))
        (b      (list nil)))
    (with-tracer (tracer)
      (with-transform (:t1)
        (recording-transform (() a) b))
      (with-transform (:t2)
        (recording-transform (() b) b)))
    (is (equal (list a) (leafs-for-source tracer a)))
    (is (equal (list b) (leafs-for-source tracer b)))))

(test roots-for-target.smoke
  "Smoke test for the `roots-for-target' functions."

  (let ((tracer (make-tracer))
        (a      (list nil))
        (b      (list nil)))
    (with-tracer (tracer)
      (with-transform (:t1)
        (recording-transform (() a) b))
      (with-transform (:t2)
        (recording-transform (() b) b)))
    (is (equal (list)   (roots-for-target tracer a)))
    (is (equal (list a) (roots-for-target tracer b)))))
