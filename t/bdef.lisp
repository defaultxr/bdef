(in-package #:bdef/tests)

(in-suite bdef-tests)

(test bdef
  "Test basic bdef functionality"
  ;; (progn
  ;;   (bdef :foo)
  ;;   (bdef :foo "/path")
  ;;   (bdef :foo "/path" :arg1 4 :arg2 99)
  ;;   (bdef "/path" :arg1 4 :arg2 99)
  ;;   (bdef "/path")
  ;;   (bdef (env (a -1 1 -1) (a 1 1)) :wavetable 1024 :arg2 99)
  ;;   (bdef :name (env (a -1 1 -1) (a 1 1)) :wavetable 1024 :arg2 99)
  ;;   )
  )

(test wavetable
  "Test wavetable functionality"
  ;; FIX: add wavetable tests
  ;; - wavetable from file
  ;; - wavetable from envelope
  ;; - :wavetable arg on already-loaded file
  ;; - wavetable from envelope with name, i.e. (bdef :foo (env ...))
  ;; - wavetable from envelope without name, i.e. (bdef (env ...)))
  )
