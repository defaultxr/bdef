(defpackage #:bdef/tests
  (:use #:cl
        #:bdef
        #:mutility
        #:cl-patterns
        #:fiveam))

(in-package #:bdef/tests)

(def-suite bdef-tests
  :description "bdef tests suite.")

(in-suite bdef-tests)

(test undocumented-symbols
  "Check for any undocumented exported symbols"
  (let ((undocumented (package-undocumented-symbols :bdef)))
    (is-false undocumented
              "Some exported symbols do not have docstrings: ~S"
              undocumented)))
