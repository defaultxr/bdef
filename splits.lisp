(in-package #:bdef)

;;; splits
;; FIX: rename point-type/type to "units"
;; FIX: can bdef be a subclass of sequence so that it can be used as the list input of prand, etc?

(defclass splits (standard-object #+sbcl sequence)
  ((starts :initarg :starts :type (vector number) :documentation "The vector of split start points. If the \"end\" slot is non-nil, each start point will be matched with an end point in the \"end\" slot with the same index.")
   (ends :initarg :ends :initform nil :type (or null (vector number)) :documentation "The vector of split end points, or NIL if no end points are defined.")
   (loops :initarg :loops :initform nil :type (or null (vector number)) :documentation "The vector of split loop points, or NIL if no loop points are defined.")
   (comments :initarg :comments :initform nil :type (or null (vector (or null string))) :documentation "The vector of comments for each split point.")
   (point-type :initarg :point-type :initform :percents :accessor splits-point-type :type symbol :documentation "Whether the splits are indexed by percents (i.e. 0.5 is in the middle of the buffer), samples, or seconds.")
   (bdef :initarg :bdef :initform nil :accessor splits-bdef :type (or null bdef) :documentation "The bdef object that this splits references (i.e. for information like duration, sample rate, etc, for converting point data).")
   (metadata :initarg :metadata :initform nil :accessor splits-metadata :documentation "Any additional metadata for the splits object."))
  (:documentation "List of split data for dividing buffers into pieces."))

(defmethod print-object ((this splits) stream)
  (format stream "#<~a~@[ :BDEF ~a~]>" 'splits (splits-bdef this)))

(defun make-splits (starts &key ends loops comments (point-type :percents) bdef metadata)
  "Make a `splits' object."
  (assert (member point-type (list :percents :samples :seconds)) (point-type))
  (assert (typep bdef '(or null bdef)))
  (make-instance 'splits
                 :starts (coerce starts 'vector)
                 :ends (when ends (coerce ends 'vector))
                 :loops (when loops (coerce loops 'vector))
                 :comments (when comments (coerce comments 'vector))
                 :point-type point-type
                 :bdef bdef
                 :metadata metadata))

(defun %splits-ensure-point-type (point)
  "Ensure POINT is one of the splits point types."
  (assert (member point (list :start :end :loop :comment :starts :ends :loops :comments)) (point))
  (ecase point
    ((:starts :start) 'starts)
    ((:ends :end) 'ends)
    ((:loops :loop) 'loops)
    ((:comments :comment) 'comments)))

(defun %splits-ensure-type (type)
  "Ensure TYPE is one of the splits position types."
  (assert (member (alexandria:make-keyword type) (list :percent :sample :frame :second :percents :samples :frames :seconds)) (type))
  (ecase (alexandria:make-keyword type)
    ((:percent :percents) 'percents)
    ((:sample :samples :frame :frames) 'samples)
    ((:second :seconds) 'seconds)))

(defun %splits-conversion-function-name (splits type)
  "Get the name of the conversion function to convert SPLITS' type into the split type specified by TYPE."
  (let ((type (%splits-ensure-type type))
        (point-type (slot-value splits 'point-type)))
    (if (string= (symbol-name type) (symbol-name point-type))
        'identity
        (intern (concatenate 'string (symbol-name point-type) "-" (symbol-name type)) 'bdef))))

(defun %splits-conversion-function (splits type)
  "Get a function that can be used to convert SPLITS' type into the split type specified by TYPE."
  (let* ((bdef (splits-bdef splits)) ;; FIX: handle the case if bdef is NIL.
         (conv-func (%splits-conversion-function-name splits type))
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
    (splits
     (length (slot-value object 'starts)))
    (bdef
     (splits-length (bdef-splits object)))))

#+sbcl
(defmethod sequence:length ((this splits))
  (splits-length this))

(defmethod frames ((this splits))
  (frames (splits-bdef this)))

(defmethod duration ((this splits))
  (duration (splits-bdef this)))

(defun splits-points (splits &optional (point :start) (type :percent))
  "Get the split points for POINTS (i.e. start, end, loops, comments) from SPLITS converted to TYPE (i.e. percent, samples, seconds)."
  (assert (typep splits 'splits) (splits))
  (assert (member point (list :start :end :loop :comment :starts :ends :loops :comments)) (point))
  (assert (member type (list :percent :sample :frame :second :percents :samples :frames :seconds)) (type))
  (let ((array (slot-value splits (%splits-ensure-point-type point)))
        (conv-func (%splits-conversion-function splits (%splits-ensure-type type))))
    (if (or (null array)
            (eq 'identity conv-func))
        array
        (map 'vector conv-func array))))

;; FIX: setf functions for these?

(defun splits-starts (splits &optional (type :percent))
  (splits-points splits :start type))

(defun splits-ends (splits &optional (type :percent))
  (splits-points splits :end type))

(defun splits-loops (splits &optional (type :percent))
  (splits-points splits :loop type))

(defun splits-comments (splits &optional (type :percent))
  (splits-points splits :comment type))

(defun splits-point (splits split &optional (point :start) (type :percent))
  "Get the split point SPLIT from SPLITS, converting to the correct TYPE (percent, samples, seconds)."
  (assert (integerp split) (split))
  (assert (member point (list :start :end :loop :comment :starts :ends :loops :comments)) (point))
  (assert (member type (list :percent :sample :frame :second :percents :samples :frames :seconds)) (type))
  (let* ((splits (if (typep splits '(or bdef symbol))
                     (bdef-splits splits)
                     splits))
         (array (slot-value splits (%splits-ensure-point-type point)))
         (conv-func (%splits-conversion-function splits type)))
    (when (null array)
      (return-from splits-point nil))
    (if (eql 'identity conv-func)
        (elt array split)
        (funcall conv-func (elt array split)))))

(defun end-point (object &optional (type :percent))
  "Get the end point of OBJECT."
  (etypecase object
    ((or bdef splits)
     (ecase type
       ((:percent :percents) 1.0)
       ((:sample :frame :samples :frames) (frames object))
       ((:second :seconds) (duration object))))))

(defun percents-samples (percent total-samples)
  (round (* percent total-samples)))

(defun samples-percents (samples total-samples)
  (/ samples total-samples))

(defun percents-seconds (percent total-seconds)
  (error "percent-seconds conversion is not done yet.") ;; FIX
  )

(defun seconds-percents (seconds total-seconds)
  (/ seconds total-seconds))

(defun seconds-samples (seconds sample-rate)
  (* seconds sample-rate))

(defun samples-seconds (samples sample-rate)
  (/ samples sample-rate))

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
    (alexandria:when-let ((metadata (ffmpeg-metadata file)))
      (dolist (tag (list :tbp :tempo :bpm :tbpm))
        (alexandria:when-let* ((get (getf metadata tag))
                               (get (parse-float:parse-float get :junk-allowed t)))
          (when (plusp get)
            (return-from extract-bpm-from-file-metadata get)))))))

;;; aubio

(defparameter *aubio-python-directory* #P"~/misc/aubio/python/demos/")

(defun aubio-onsets-read (file &rest args &key (algorithm :default) (threshold 0.3) (silence -90) &allow-other-keys)
  "Use the \"aubioonset\" utility to get a list of onsets in FILE. FILE can be a path to a file or a cl-collider buffer object. The returned list gives onsets in seconds from the start of the file."
  ;; FIX: use the algorithm, threshold, and silence keys
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
  (apply 'make-splits
         (coerce (apply 'aubio-onsets-read file args) 'vector) ;; FIX: just generate as a vector
         :point-type :seconds
         (when (typep file 'bdef)
           (list :bdef file))))

(defun aubio-demo-bpm (file &key (mode :default))
  "Use aubio's demo_bpm_extract.py to get the bpm of FILE."
  (assert (member mode '(:default :fast :super-fast)) (mode))
  (let* ((file (ensure-readable-audio-file (path file)))
         (string (uiop:run-program (list "python" (namestring (truename (merge-pathnames *aubio-python-directory* "demo_bpm_extract.py"))) "-m" (string-downcase (string mode)) (namestring (truename file)))
                                   :output '(:string :stripped t))))
    (car (multiple-value-list (read-from-string (subseq string 0 (position #\space string)))))))

(defun aubio-demo-tempo (file &key sample-rate)
  "Use aubio's demo_tempo.py to get a list of beats in FILE. The returned list gives onsets in seconds from the start of the file."
  ;; FIX: use the sample-rate key
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

(defun aubio-track-to-audacity-labels (file)
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
                 (princ (concatenate 'string last #\tab time #\tab i #\newline) s)
                 (setf last time)))))))

;;; bpm-tools

(defun bpm-tools-bpm (file)
  "Use bpm-tools' \"bpm\" utility to get the BPM of FILE."
  (values (read-from-string (uiop:run-program (format nil "sox -V1 \"~a\" -r 44100 -e float -c 1 -t raw - | bpm" file)
                                              :force-shell t :output '(:string :stripped t)))))

;;; audacity labels

(defun splits-from-audacity-labels (labels)
  "Make a `splits' from an Audacity labels file."
  (typecase labels
    (string (audacity-labels (pathname labels)))
    (pathname
     (with-open-file (stream labels :direction :input :if-does-not-exist :error)
       (loop :for line = (read-line stream nil)
          :while line
          :for parsed = (split-sequence:split-sequence #\tab line :remove-empty-subseqs t)
          :collect (parse-float:parse-float (car parsed)) :into starts
          :collect (parse-float:parse-float (cadr parsed)) :into ends
          :collect (caddr parsed) :into comments
          :finally (return (make-splits starts :ends ends :comments comments :point-type :seconds)))))))

;;; op-1 drumsets
;; https://github.com/padenot/libop1/blob/master/src/op1_drum_impl.cpp

;;; snd marks
;; TODO

;;; echo nest / amen
;; https://github.com/algorithmic-music-exploration/amen
;; TODO

;;; acousticbrainz
;; https://beets.readthedocs.io/en/v1.4.7/plugins/acousticbrainz.html
;; TODO

