(in-package #:bdef/tests)

(in-suite bdef-tests)

(test splits-conversions
  "Test splits unit conversion functions"
  (is (= 100 (bdef::percents-samples 0.5 200)))
  (is (= 16 (bdef::percents-samples 0.5 33)))
  (is (= 1/2 (bdef::samples-percents 100 200)))
  (is (= 150 (bdef::percents-seconds 0.5 300)))
  (is (= 1/3 (bdef::seconds-percents 3 9)))
  (is (= 88200 (bdef::seconds-samples 2 44100)))
  (is (= 1/2 (bdef::samples-seconds 24000 48000))))

(test op-1
  "Test OP-1 format import/export functions"
  (is (= 40 (bdef::op-1-format-to-frame (bdef::frame-to-op-1-format 40)))))
