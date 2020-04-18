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

(test bdef
  "Test basic bdef functionality"
  ;; (progn
  ;;   (fresh-line)
  ;;   (bdef :foo)
  ;;   (bdef :foo "/path")
  ;;   (bdef :foo "/path" :arg1 4 :arg2 99)
  ;;   (bdef "/path" :arg1 4 :arg2 99)
  ;;   (bdef "/path")
  ;;   (bdef (env (a -1 1 -1) (a 1 1)) :wavetable 1024 :arg2 99)
  ;;   (bdef :name (env (a -1 1 -1) (a 1 1)) :wavetable 1024 :arg2 99)
  ;;   )
  )

(test op-1
  "Test OP-1 format import/export functions"
  (is (= 40 (bdef::frame-to-op-1-format (bdef::op-1-format-to-frame 40)))))

