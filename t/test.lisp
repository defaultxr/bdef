(defpackage #:bdef/tests
  (:use #:cl
        #:bdef
        #:mutility
        #:fiveam))

(in-package #:bdef/tests)

(def-suite bdef-tests
  :description "bdef tests suite.")

(in-suite bdef-tests)

(test undocumented-symbols
  "Check for any undocumented exported symbols"
  (let ((undocumented (undocumented-symbols :bdef)))
    (is-false undocumented
              "some exported symbols do not have docstrings: ~s"
              undocumented)))
