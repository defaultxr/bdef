(in-package #:bdef/tests)

(in-suite bdef-tests)

(test psplits
  "Test psplits"
  (is (every-event-equal (list (event :start 0 :end 0.25)
                               (event :start 0.25 :end 1)
                               eop eop eop)
                         (next-n (psplits (make-splits (list 0 0.25) :ends (list 0.25 1) :unit :percents)
                                          (pseq (list 0 1) 1))
                                 5))))
