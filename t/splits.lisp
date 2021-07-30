(in-package #:bdef/tests)

(in-suite bdef-tests)

(test op-1
  "Test OP-1 format import/export functions"
  (is (= 40 (bdef::op-1-format-to-frame (bdef::frame-to-op-1-format 40)))))
