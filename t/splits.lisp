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

(test string-extract-bpm
  "Test functionality to extract BPMs from a string"
  (is (= 250 (bdef::string-extract-bpm "120 130 240bpm 250bpm 270")))
  (is (= 280 (bdef::string-extract-bpm "120 99 240bpm 280BPM 270")))
  (is (= 260.5 (bdef::string-extract-bpm "120 260.5bpm 270 ehllo")))
  (is (= 140 (bdef::string-extract-bpm "120 test test 10 140 bpm")))
  (is (= 90 (bdef::string-extract-bpm "my-cool-breakbeat-at-90-bpm")))
  (is (= 100 (bdef::string-extract-bpm "100bpm-breakbeat")))
  (is (= 120 (bdef::string-extract-bpm "loop-120")))
  (is (null (bdef::string-extract-bpm "loop-1"))))

(test op-1
  "Test OP-1 format import/export functions"
  (is (= 40 (bdef::op-1-format-to-frame (bdef::frame-to-op-1-format 40)))))
