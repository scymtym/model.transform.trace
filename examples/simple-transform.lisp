(cl:defpackage #:model.transform.trace.examples.simple-transform
  (:use
   #:cl
   #:alexandria

   #:model.transform.trace))

(cl:in-package #:model.transform.trace.examples.simple-transform)

(defun tracing-1+ (x)
  (ensured-transform (((make-symbol "TRACING-1+")) x)
    (1+ x)))

(defun tracing-+ (x y)
  (ensured-transform (((make-symbol "TRACING-+")) x y)
    (+ x y)))

(defun computation ()
  (let* ((intermediate (tracing-1+ (tracing-+ 1 2)))
         (result       (tracing-+ (tracing-1+ (tracing-1+ intermediate))
                                  (tracing-+ intermediate intermediate))))
    result))

(with-tracer ((make-instance 'tracer))
  (let* ((intermediate (tracing-1+ (tracing-+ 1 2)))
         (result       (tracing-+ (tracing-1+ (tracing-1+ intermediate))
                                  (tracing-+ intermediate intermediate))))

    ;;
    (labels ((visit (recurse transform source target &optional (depth 1))
               (format t "~V<~>~S <-~A- ~S~%" (* 2 depth) target transform source)
               (funcall recurse :function (rcurry #'visit (1+ depth)))))

      (format t "~2&~S~%" result)
      (walk-sources-for-target* #'visit result)

      (format t "~2&~S~%" result)
      (walk-unique-sources-for-target* #'visit result))

    (print (roots-for-target* result))

    ;;
    (model.transform.trace.graph:transform-graph *tracer*)
    *tracer*))
