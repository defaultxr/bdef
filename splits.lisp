(in-package #:bdef)

;;; conversions

(defun percents-samples (percent total-samples)
  (round (* percent total-samples)))

(defun samples-percents (samples total-samples)
  (/ samples total-samples))

(defun percents-seconds (percent total-seconds)
  (* percent total-seconds))

(defun seconds-percents (seconds total-seconds)
  (/ seconds total-seconds))

(defun seconds-samples (seconds sample-rate)
  (* seconds sample-rate))

(defun samples-seconds (samples sample-rate)
  (/ samples sample-rate))

;;; splits
;; FIX: add more extensible-sequences methods

(defclass splits (standard-object #+#.(cl:if (cl:find-package "SEQUENCE") '(:and) '(:or)) sequence)
  ((starts :initarg :starts :type (vector number) :documentation "The vector of split start points. If the \"end\" slot is non-nil, each start point will be matched with an end point in the \"end\" slot with the same index.")
   (ends :initarg :ends :initform nil :type (or null (vector number)) :documentation "The vector of split end points, or NIL if no end points are defined.")
   (loops :initarg :loops :initform nil :type (or null (vector number)) :documentation "The vector of split loop points, or NIL if no loop points are defined.")
   (comments :initarg :comments :initform nil :type (or null (vector (or null string))) :documentation "The vector of comments for each split point.")
   (unit :initarg :unit :initform :percents :accessor splits-unit :type symbol :documentation "The unit type of each split point, i.e. percents, samples, or seconds.")
   (bdef :initarg :bdef :initform nil :accessor splits-bdef :type (or null bdef) :documentation "The bdef object that this splits references (i.e. for information like duration, sample rate, etc, for converting point data).")
   (metadata :initarg :metadata :initform nil :accessor splits-metadata :documentation "Any additional metadata for the splits object."))
  (:documentation "List of split data for dividing buffers into pieces."))

(defmethod print-object ((this splits) stream)
  (format stream "#<~a~@[ :BDEF ~a~]>" 'splits (splits-bdef this)))

(defun make-splits (starts &key ends loops comments (unit :percents) bdef metadata)
  "Make a `splits' object. Ends, loops, and comments for each split can be specified with the keyword arguments or as sublists in STARTS.

Examples:

;; (make-splits (list (list 0 1 \"first split\") (list 1 2 \"second split\")) :unit seconds)
;; ;; ...is equivalent to:
;; (make-splits (list 0 1) :ends (list 1 2) :comments (list \"first split\" \"second split\") :unit :seconds)

See also: `splits', `splits-points', `splits-starts', `splits-ends', `splits-loops', `splits-comments'"
  (assert (member unit (list :percents :samples :seconds)) (unit))
  (assert (typep bdef '(or null bdef)))
  (let* ((list (mapcar #'ensure-list starts))
         (comments (or comments (mapcar (lambda (item)
                                          (car (remove-if-not
                                                (lambda (x) (or (stringp x)
                                                                (symbolp x)))
                                                item)))
                                        list)))
         (list (remove-if #'stringp list))
         (ends (or ends (mapcar #'cadr list)))
         (ends (if (find-if-not #'null ends) ends nil))
         (loops (or loops (mapcar #'caddr list)))
         (loops (if (find-if-not #'null loops) loops nil))
         (starts (mapcar #'car list)))
    (make-instance 'splits
                   :starts (coerce starts 'vector)
                   :ends (when ends (coerce ends 'vector))
                   :loops (when loops (coerce loops 'vector))
                   :comments (when comments (coerce comments 'vector))
                   :unit unit
                   :bdef bdef
                   :metadata metadata)))

(defun %splits-ensure-point-type (point)
  "Ensure POINT is one of the splits point types."
  (assert (member point (list :start :end :loop :comment :starts :ends :loops :comments)) (point))
  (ecase point
    ((:starts :start) 'starts)
    ((:ends :end) 'ends)
    ((:loops :loop) 'loops)
    ((:comments :comment) 'comments)))

(defun %splits-ensure-unit (unit)
  "Ensure UNIT is one of the splits position unit types."
  (assert (member (make-keyword unit) (list :percent :sample :frame :second :percents :samples :frames :seconds)) (unit))
  (ecase (make-keyword unit)
    ((:percent :percents) 'percents)
    ((:sample :samples :frame :frames) 'samples)
    ((:second :seconds) 'seconds)))

(defun %splits-conversion-function-name (splits unit)
  "Get the name of the conversion function to convert SPLITS' unit type into the unit specified."
  (let ((unit (%splits-ensure-unit unit))
        (unit-slot (slot-value splits 'unit)))
    (if (string= (symbol-name unit) (symbol-name unit-slot))
        'identity
        (intern (concatenate 'string (symbol-name unit-slot) "-" (symbol-name unit)) 'bdef))))

(defun %splits-conversion-function (splits unit)
  "Get a function that can be used to convert SPLITS' unit into the unit type specified."
  (let* ((bdef (splits-bdef splits)) ;; FIX: handle the case if bdef is NIL.
         (conv-func (%splits-conversion-function-name splits unit))
         (conv-func-total (case conv-func
                            ((percents-samples samples-percents) (frames bdef))
                            ((samples-seconds seconds-samples) (sample-rate bdef))
                            ((percents-seconds seconds-percents) (duration bdef)))))
    (if (eql 'identity conv-func)
        'identity
        (lambda (x) (funcall conv-func x conv-func-total)))))

(defun splits-length (object)
  "Get the number of splits defined in OBJECT."
  (etypecase object
    (symbol
     (splits-length (bdef object)))
    (splits
     (length (slot-value object 'starts)))
    (bdef
     (splits-length (bdef-splits object)))))

#+#.(cl:if (cl:find-package "SEQUENCE") '(:and) '(:or))
(defmethod sequence:length ((this splits))
  (splits-length this))

(defmethod frames ((this splits))
  (frames (splits-bdef this)))

(defmethod duration ((this splits))
  (duration (splits-bdef this)))

(defun splits-points (splits &optional (point :start) (unit :percent))
  "Get the split points for POINTS (i.e. start, end, loops, comments) from SPLITS converted to UNIT (i.e. percent, samples, seconds)."
  (assert (typep splits 'splits) (splits))
  (assert (member point (list :start :end :loop :comment :starts :ends :loops :comments)) (point))
  (assert (member unit (list :percent :sample :frame :second :percents :samples :frames :seconds)) (unit))
  (let ((array (slot-value splits (%splits-ensure-point-type point)))
        (conv-func (%splits-conversion-function splits (%splits-ensure-unit unit))))
    (if (or (null array)
            (eq 'identity conv-func))
        array
        (map 'vector conv-func array))))

;; FIX: setf functions for these?

(defun splits-starts (splits &optional (unit :percent))
  (splits-points splits :start unit))

(defun splits-ends (splits &optional (unit :percent))
  (splits-points splits :end unit))

(defun splits-loops (splits &optional (unit :percent))
  (splits-points splits :loop unit))

(defun splits-comments (splits &optional (unit :percent))
  (splits-points splits :comment unit))

(defun splits-point (splits split &optional (point :start) (unit :percent))
  "Get the split point SPLIT from SPLITS, converting to the correct UNIT (percent, samples, seconds)."
  (assert (integerp split) (split))
  (assert (member point (list :start :end :loop :comment :starts :ends :loops :comments)) (point))
  (assert (member unit (list :percent :sample :frame :second :percents :samples :frames :seconds)) (unit))
  (let* ((splits (if (typep splits '(or bdef symbol))
                     (bdef-splits splits)
                     splits))
         (array (slot-value splits (%splits-ensure-point-type point)))
         (conv-func (%splits-conversion-function splits unit)))
    (when (null array)
      (return-from splits-point nil))
    (if (eql 'identity conv-func)
        (elt array split)
        (funcall conv-func (elt array split)))))

(defun end-point (object &optional (unit :percent))
  "Get the end point of OBJECT."
  (etypecase object
    ((or bdef splits)
     (ecase unit
       ((:percent :percents) 1.0)
       ((:sample :frame :samples :frames) (frames object))
       ((:second :seconds) (duration object))))))

;;; splits / analysis methods

;; filename / metadata

(defun extract-bpm-from-string (string)
  "Extract the BPM from a string (typically a filename). Returns NIL if no BPM-like string is found.

NOTE: If \"bpm\" is not in the string, then this function will look for a number in the range 50-400, starting from the end of the string."
  (when string
    ;; (warn "extract-bpm-from-string is not done yet!")
    (let* ((split (split-sequence:split-sequence-if (lambda (c) (member c (list #\space #\_ #\-) :test 'char=)) string :remove-empty-subseqs t))
           (bpm-pos (position-if (lambda (s)
                                   (search "bpm" (string-downcase s)))
                                 split
                                 :from-end t)))
      (values (if bpm-pos
                  (or (parse-float:parse-float (elt split bpm-pos) :junk-allowed t)
                      (parse-float:parse-float (elt split (1- bpm-pos)) :junk-allowed t))
                  (loop :for i :in (nreverse split)
                     :for bpm = (parse-float:parse-float i :junk-allowed t)
                     :if (and (numberp bpm)
                              (>= 400 bpm 50))
                     :return bpm))))))

(defun extract-bpm-from-file-metadata (file) ;; FIX: this should be used on the source too (i.e. mp3 files) in case there is a BPM value in the tags.
  "Extract the BPM from the metadata embedded in the file using ffmpeg to read it."
  (when file
    (when-let ((metadata (ffmpeg-metadata file)))
      (dolist (tag (list :tbp :tempo :bpm :tbpm))
        (when-let* ((get (getf metadata tag))
                    (get (parse-float:parse-float get :junk-allowed t)))
          (when (plusp get)
            (return-from extract-bpm-from-file-metadata get)))))))

;;; aubio

(defparameter *aubio-python-directory* #P"~/misc/aubio/python/demos/")

;; FIX: maybe just make a general function for all the aubio commands?
(defun aubio-onsets-read (file &rest args &key (algorithm :default) (threshold 0.3) (silence -90) &allow-other-keys)
  "Use the \"aubioonset\" utility to get a list of onsets in FILE. FILE can be a path to a file or a cl-collider buffer object. The returned list gives onsets in seconds from the start of the file."
  (declare (ignore algorithm threshold silence args)) ;; FIX: actually implement these arguments
  ;; FIX: uiop:run-program allows you to read output as a stream. that might be better?
  (let ((file (ensure-readable-audio-file
               (if (or (stringp file)
                       (pathnamep file))
                   file
                   (path file)))))
    (labels ((split (string &optional list)
               (let ((start (position #\newline string)))
                 (if start
                     (split (subseq string (1+ start)) (append list (list (subseq string 0 start))))
                     (append list (list string))))))
      (mapcar #'read-from-string ;; use "aubiocut -b FILE" to get a file cut by beats
              (split (uiop:run-program (list "aubioonset" (namestring (truename file)))
                                       :output '(:string :stripped t)))))))

(defun splits-from-aubio-onsets (file &rest args &key &allow-other-keys) ;; FIX: make sure this works with bdefs
  "Make a `splits' from onsets data generated by Aubio."
  (apply 'make-splits
         (coerce (apply 'aubio-onsets-read file args) 'vector) ;; FIX: just generate as a vector?
         :unit :seconds
         (when (typep file 'bdef)
           (list :bdef file))))

(defun splits-from-aubio-track (file &key (buf-size 512) (hop-size 256) (silence-threshold -90)) ;; FIX: make a more general aubio function for any of its commands?
  (make-splits (mapcar 'parse-float:parse-float
                       (split-sequence:split-sequence
                        #\newline
                        (uiop:run-program (list "aubiotrack"
                                                "-i" (namestring (truename file))
                                                "-B" (write-to-string buf-size)
                                                "-H" (write-to-string hop-size)
                                                "-s" (write-to-string silence-threshold)
                                                "-T" "samples")
                                          :output '(:string :stripped t))
                        :remove-empty-subseqs t))
               :unit :samples))

(defun aubio-demo-bpm (file &key (mode :default))
  "Use aubio's demo_bpm_extract.py to get the bpm of FILE."
  (assert (member mode '(:default :fast :super-fast)) (mode))
  (let* ((file (ensure-readable-audio-file (path file)))
         (string (uiop:run-program (list "python" (namestring (truename (merge-pathnames *aubio-python-directory* "demo_bpm_extract.py"))) "-m" (string-downcase (string mode)) (namestring (truename file)))
                                   :output '(:string :stripped t))))
    (car (multiple-value-list (read-from-string (subseq string 0 (position #\space string)))))))

(defun aubio-demo-tempo (file &key sample-rate)
  "Use aubio's demo_tempo.py to get a list of beats in FILE. The returned list gives onsets in seconds from the start of the file."
  (declare (ignore sample-rate)) ;; FIX: actually use the sample-rate key
  ;; FIX: uiop:run-program allows you to read output as a stream. that might be better?
  (let ((file (ensure-readable-audio-file (path file))))
    (labels ((split (string &optional list)
               (let ((start (position #\newline string)))
                 (if start
                     (split (subseq string (1+ start)) (append list (list (subseq string 0 start))))
                     (append list (list string))))))
      (mapcar #'read-from-string ;; use "aubiocut -b FILE" to get a file cut by beats
              (split (uiop:run-program (list "python" (namestring (merge-pathnames *aubio-python-directory* "demo-tempo.py")) (namestring (truename file)))
                                       :output '(:string :stripped t)))))))

(defgeneric splits-export (splits file format)
  (:documentation "Write a `splits' object to a file in another format."))

(defmethod splits-export ((bdef bdef) file format)
  (splits-export (bdef-splits bdef) file format))

(defmethod splits-export (object (file string) format)
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (splits-export object stream format)))

(defmethod splits-export (object (file pathname) format)
  (splits-export object (namestring (truename file)) format))

(defmethod splits-export ((splits splits) (stream stream) (format (eql :audacity)))
  (loop :for idx :from 0 :below (length splits)
     :for start = (splits-point splits idx :start :second)
     :for end = (or (splits-point splits idx :end :second)
                    (ignore-errors (splits-point splits (1+ idx) :start :second)) ;; FIX: just use ensure-end, (or maybe integrate ensure-end into the regular splits-ends function?)
                    (end-point splits :second))
     :for comment = (splits-point splits idx :comment)
     :do (format stream "~a~c~a~c~a~%" start #\tab end #\tab (or comment idx))))

(defun aubio-track-to-audacity-labels (file) ;; FIX: remove this and just make a function to generate a `splits' from aubiotrack, since `audacity-labels-from-splits' already exists.
  "Generate an Audacity labels file from the output of aubio's aubiotrack on FILE."
  (let ((beats-times (mapcar 'parse-float:parse-float
                             (split-sequence:split-sequence
                              #\newline
                              (uiop:run-program (list "aubiotrack"
                                                      "-B" "4096"
                                                      "-H" "128"
                                                      "-s" "-20"
                                                      (namestring (truename file)))
                                                :output '(:string :stripped t))
                              :remove-empty-subseqs t))))
    (with-open-file (s #P"~/label.txt" :direction :output :if-exists :supersede)
      (let ((last 0.0))
        (loop :for time :in beats-times
           :for i :from 0 :below (length beats-times)
           :do (progn
                 (format s "~s~c~s~c~s~%" last #\tab time #\tab i)
                 (setf last time)))))))

;;; bpm-tools

(defun bpm-tools-bpm (file)
  "Use bpm-tools' \"bpm\" utility to get the BPM of FILE."
  (values (read-from-string (uiop:run-program (format nil "sox -V1 \"~a\" -r 44100 -e float -c 1 -t raw - | bpm" file)
                                              :force-shell t :output '(:string :stripped t)))))

;;; audacity labels

(defun splits-from-audacity-labels (labels)
  "Make a `splits' from an Audacity labels file."
  (with-open-file (stream labels :direction :input :if-does-not-exist :error)
    (loop :for line = (read-line stream nil)
       :while line
       :for parsed = (split-sequence:split-sequence #\tab line :remove-empty-subseqs t)
       :collect (parse-float:parse-float (car parsed)) :into starts
       :collect (parse-float:parse-float (cadr parsed)) :into ends
       :collect (caddr parsed) :into comments
       :finally (return (make-splits starts :ends ends :comments comments :unit :seconds)))))

(defun audacity-labels-from-splits (splits &optional (file #P"~/label.txt"))
  "Export a `splits' object as an Audacity labels file."
  (labels ((writer (stream)
             (loop :for idx :from 0 :below (length splits)
                :for start = (splits-point splits idx :start :second)
                :for end = (or (splits-point splits idx :end :second)
                               (ignore-errors (splits-point splits (1+ idx) :start :second))
                               (end-point splits :second))
                :for comment = (splits-point splits idx :comment)
                :do (format stream "~a~c~a~c~a~%" start #\tab end #\tab (or comment idx)))))
    (etypecase file
      ((or pathname string)
       (with-open-file (stream file :direction :output :if-exists :supersede)
         (writer stream)))
      (stream
       (writer file)))))

;;; op-1 drumsets
;; https://github.com/padenot/libop1/blob/master/src/op1_drum_impl.cpp

(defconstant +op-1-drumkit-end+ #x7FFFFFFE)

(defconstant +op-1-bytes-in-12-sec+ (* 44100 2 12))

(defconstant +size-of-uint16-t+ 2)

(defun frame-to-op-1-format (frame)
  "Convert a frame number to the OP-1 split point format.

See also: `op-1-format-to-frame'"
  (round
   (/ +op-1-drumkit-end+
      (* +op-1-bytes-in-12-sec+ frame +size-of-uint16-t+))))

(defun op-1-format-to-frame (op-1-split)
  "Convert a number in OP-1 split point format to a frame number.

See also: `frame-to-op-1-format'."
  (round (/ (* (/ op-1-split +op-1-drumkit-end+)
               +size-of-uint16-t+ +op-1-bytes-in-12-sec+)
            4)))

(defun frame-to-op-1-format.new (frame)
  "Convert a frame number to the OP-1 split point format.

See also: `op-1-format-to-frame'"
  (round
   (* (/ +op-1-drumkit-end+
         +op-1-bytes-in-12-sec+)
      frame +size-of-uint16-t+)))

(defun op-1-format-to-frame.new (op-1)
  "Convert a frame number to the OP-1 split point format.

See also: `op-1-format-to-frame'"
  (round
   (* (/ +op-1-bytes-in-12-sec+
         +op-1-drumkit-end+)
      op-1 +size-of-uint16-t+)))

#|
(defun splits-from-op-1-drumset (drumset)
"Make a `splits' from an OP-1 format drumset."
(etypecase drumset
(bdef
(let ((splits (splits-from-op-1-drumset (path drumset))))
(setf (splits-bdef splits) drumset)
splits))
(string
(with-open-file (stream drumset :direction :input)
(let ((array (make-array 4 :initial-element nil)))
(loop :for peek = (peek-char stream nil nil)
:if (null peek)
:do (progn
(print 'couldnt-find-it)
(loop-finish))
:if (char= #\o peek)
:do (progn
(read-sequence array stream)
(when (equalp #(#\o #\p #\- #\1) array)
(print 'found-it)))
:do (read-char stream nil nil)))
;; (make-splits starts :ends ends :unit :frames)
))))
|#

;;; snd marks
;; TODO

;;; echo nest / amen
;; https://github.com/algorithmic-music-exploration/amen
;; TODO

;;; acousticbrainz
;; https://beets.readthedocs.io/en/v1.4.7/plugins/acousticbrainz.html
;; TODO

