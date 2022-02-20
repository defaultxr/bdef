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

(defclass splits (standard-object #+#.(cl:if (cl:find-package "SEQUENCE") '(:and) '(:or)) sequence)
  ((starts :initarg :starts :type (vector number) :documentation "The vector of split start points. If the \"end\" slot is non-nil, each start point will be matched with an end point in the \"end\" slot with the same index.")
   (ends :initarg :ends :initform nil :type (or null (vector number)) :documentation "The vector of split end points, or NIL if no end points are defined.")
   (loops :initarg :loops :initform nil :type (or null (vector number)) :documentation "The vector of split loop points, or NIL if no loop points are defined.")
   (comments :initarg :comments :initform nil :type (or null (vector (or null string))) :documentation "The vector of comments for each split point.")
   (unit :initarg :unit :initform :percents :accessor splits-unit :type symbol :documentation "The unit type of each split point, i.e. percents, samples, or seconds.")
   (bdef :initarg :bdef :initform nil :accessor splits-bdef :type (or null bdef) :documentation "The bdef object that this splits references (i.e. for information like duration, sample rate, etc, for converting point data).")
   (metadata :initarg :metadata :initform (make-hash-table) :type hash-table :documentation "Hash table of additional data associated with the splits, accessible with the `splits-metadata' function."))
  (:documentation "List of split data for dividing buffers into pieces."))

(defmethod print-object ((splits splits) stream)
  (format stream "#<~a~@[ :BDEF ~a~]>" 'splits (splits-bdef splits)))

(defun splits-p (object)
  "True if OBJECT is a splits.

See also: `splits', `make-splits'"
  (typep object 'splits))

(defun make-splits (starts &key ends loops comments (unit :percents) bdef metadata)
  "Make a `splits' object. Ends, loops, and comments for each split can be specified with the keyword arguments or as sublists in STARTS.

Examples:

;; (make-splits (list (list 0 1 \"first split\") (list 1 2 \"second split\")) :unit seconds)
;; ;; ...is equivalent to:
;; (make-splits (list 0 1) :ends (list 1 2) :comments (list \"first split\" \"second split\") :unit :seconds)

See also: `splits', `splits-points', `splits-starts', `splits-ends', `splits-loops', `splits-comments'"
  (assert (typep bdef '(or null bdef)))
  (let* ((listp (listp (elt starts 0)))
         (comments (or comments (and listp
                                     (mapcar (lambda (item)
                                               (find-if (fn (typep _ 'string-designator)) item))
                                             starts))))
         (list (when listp
                 (mapcar (lambda (x) (remove-if #'stringp x)) starts)))
         (ends (or ends (and listp
                             (mapcar #'cadr list))))
         (ends (when (find-if-not #'null ends) ends))
         (loops (or loops (and listp
                               (mapcar #'caddr list))))
         (loops (when (find-if-not #'null loops) loops))
         (starts (if listp
                     (mapcar #'car list)
                     starts)))
    (make-instance 'splits
                   :starts (coerce starts 'vector)
                   :ends (when ends (coerce ends 'vector))
                   :loops (when loops (coerce loops 'vector))
                   :comments (when comments (coerce comments 'vector))
                   :unit (%splits-ensure-unit unit)
                   :bdef bdef
                   :metadata (etypecase metadata
                               (list (plist-hash-table metadata))
                               (hash-table metadata)))))

(defun %splits-ensure-point-type (point)
  "Ensure POINT is one of the splits point types."
  (ecase point
    ((:starts :start) 'starts)
    ((:ends :end) 'ends)
    ((:loops :loop) 'loops)
    ((:comments :comment) 'comments)))

(defun %splits-ensure-unit (unit)
  "Ensure UNIT is one of the splits position unit types."
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
        (intern (concat (symbol-name unit-slot) "-" (symbol-name unit)) 'bdef))))

(defun %splits-conversion-function (splits unit)
  "Get a function that can be used to convert SPLITS' unit into the unit type specified."
  (let* ((unit (%splits-ensure-unit unit))
         (conv-func (%splits-conversion-function-name splits unit)))
    (if (eql 'identity conv-func)
        'identity
        (lambda (x) (funcall conv-func x (case conv-func
                                           ((percents-samples samples-percents) (bdef-length splits))
                                           ((samples-seconds seconds-samples) (bdef-sample-rate splits))
                                           ((percents-seconds seconds-percents) (bdef-duration splits))))))))

(defun splits-length (object)
  "Get the number of splits defined in OBJECT."
  (etypecase object
    (symbol
     (splits-length (bdef object)))
    (splits
     (length (slot-value object 'starts)))
    (bdef
     (splits-length (bdef-splits object)))))

(defun splits-points (splits &optional (point :start) (unit :percents))
  "Get the split points for POINTS (i.e. start, end, loops, comments) from SPLITS converted to UNIT (i.e. percent, samples, seconds)."
  (assert (typep splits 'splits) (splits))
  (assert (member point (list :start :end :loop :comment :starts :ends :loops :comments)) (point))
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

(defun splits-point (splits split &optional (point :start) (unit :percents))
  "Get the split point SPLIT from SPLITS, converting to the correct UNIT (percent, samples, seconds)."
  (assert (integerp split) (split))
  (assert (member point (list :start :end :loop :comment :starts :ends :loops :comments)) (point))
  (let* ((splits (if (typep splits '(or bdef symbol))
                     (bdef-splits splits)
                     splits))
         (unit (%splits-ensure-unit unit))
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
       ((:sample :frame :samples :frames) (bdef-length object))
       ((:second :seconds) (bdef-duration object))))))

(defgeneric splits-metadata (splits &optional key)
  (:documentation "Get the value of SPLITS's metadata for KEY, or the full metadata hashtable if KEY is not provided. Returns true as a second value if the metadata had an entry for KEY, or false if it did not."))

(defmethod splits-metadata ((splits splits) &optional key)
  (with-slots (metadata) splits
    (if key
        (gethash key metadata)
        metadata)))

(defmethod (setf splits-metadata) (value (splits splits) &optional key)
  (with-slots (metadata) splits
    (if key
        (progn
          (when (typep value 'bdef)
            (unless (bdef-splits value)
              (setf (bdef-splits value) splits))
            (unless (splits-bdef splits)
              (setf (splits-bdef splits) value)))
          (setf (gethash key metadata) value))
        (setf metadata (etypecase value
                         (list (plist-hash-table value))
                         (hash-table value))))))

(defgeneric splits-export (splits file format)
  (:documentation "Write a `splits' object to a file in another format."))

(defmethod splits-export ((bdef bdef) file format)
  (splits-export (bdef-splits bdef) file format))

(defmethod splits-export (object (file string) format)
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (splits-export object stream format)))

(defmethod splits-export (object (file pathname) format)
  (splits-export object (namestring (truename file)) format))

(defmethod bdef-buffer ((this splits))
  (bdef-buffer (splits-bdef this)))

(defmethod bdef-length ((this splits))
  (bdef-length (splits-bdef this)))

(defmethod bdef-sample-rate ((this splits))
  (bdef-sample-rate (splits-bdef this)))

(defmethod bdef-channels ((this splits))
  (bdef-channels (splits-bdef this)))

(defmethod bdef-id ((this splits))
  (bdef-id (splits-bdef this)))

(defmethod bdef-file ((this splits))
  (bdef-file (splits-bdef this)))

(defmethod bdef-frames ((this splits) &key start end channels)
  (bdef-frames (splits-bdef this) :start start :end end :channels channels))

;;; splits / analysis methods

;; filename / metadata

(defun extract-bpm-from-string (string)
  "Extract the BPM from a string (typically a filename). Returns NIL if no BPM-like string is found.

NOTE: If \"bpm\" is not in the string, then this function will look for a number in the range 50-400, starting from the end of the string."
  (when string
    ;; (warn "extract-bpm-from-string is not done yet!")
    (let* ((split (string-split string :char-bag (list #\space #\_ #\-)))
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

;;; aubio

(defvar *aubio-python-directory* #P"~/misc/aubio/python/demos/" ;; FIX: can we do without this?
        "The path to Aubio's \"demos\" directory.")

(defun aubio-onsets (file &rest args &key (utility :onset) (algorithm :hfc) (threshold 0.3) (silence -90) (minimum-interval 0.012) (buf-size 512) (hop-size 256) &allow-other-keys)
  "Use the \"aubioonset\" or \"aubiotrack\" utility to get a list of onset points in FILE in seconds. FILE can be a filename, a bdef, or a supported buffer object. UTILITY specifies whether to use Aubio's \"onset\" or \"track\" utility. The rest of the arguments are sent as parameters to the Aubio utility in use; see Aubio's documentation for more information on them."
  (declare (ignore args))
  (check-type threshold float)
  (check-type silence number)
  (check-type minimum-interval (real 0))
  (check-type buf-size (integer 0))
  (check-type hop-size (integer 0))
  (mapcar #'parse-float:parse-float
          (string-split
           (uiop:run-program (list (ecase utility
                                     (:onset "aubioonset")
                                     (:track "aubiotrack"))
                                   "--input" (ensure-readable-audio-file
                                              (uiop:native-namestring (if (bdef-p file)
                                                                          (bdef-file file)
                                                                          file)))
                                   "--bufsize" (write-to-string buf-size)
                                   "--hopsize" (write-to-string hop-size)
                                   "--onset" (string-downcase algorithm)
                                   "--onset-threshold" (write-to-string threshold)
                                   "-M" (write-to-string minimum-interval)
                                   "--silence" (write-to-string silence))
                             :output (list :string :stripped t))
           :char-bag (list #\newline))))

(defun splits-from-aubio (file &rest args &key &allow-other-keys)
  "Make a `splits' from onsets data generated by Aubio by using the `aubio-onsets' function.

See also: `splits', `aubio-onsets'"
  (apply #'make-splits
         (apply #'aubio-onsets file args)
         :unit :seconds
         (when (bdef-p file)
           (list :bdef file))))

(uiop:with-deprecation (:style-warning)
  (defun splits-from-aubio-onsets (file &rest args &key &allow-other-keys)
    "Deprecated alias for (splits-from-aubio FILE :utility :onset ...)."
    (apply #'splits-from-aubio file :utility :onset args)))

(defun aubio-demo-bpm (file &key (mode :default))
  "Use aubio's demo_bpm_extract.py to get the bpm of FILE."
  (assert (member mode '(:default :fast :super-fast)) (mode))
  (let* ((file (ensure-readable-audio-file (bdef-file file)))
         (string (uiop:run-program (list "python" (namestring (truename (merge-pathnames *aubio-python-directory* "demo_bpm_extract.py"))) "-m" (string-downcase (string mode)) (namestring (truename file)))
                                   :output '(:string :stripped t))))
    (car (multiple-value-list (read-from-string (subseq string 0 (position #\space string)))))))

(defun aubio-demo-tempo (file &key sample-rate)
  "Use aubio's demo_tempo.py to get a list of beats in FILE. The returned list gives onsets in seconds from the start of the file."
  (declare (ignore sample-rate)) ;; FIX: actually use the sample-rate key
  ;; FIX: uiop:run-program allows you to read output as a stream. that might be better?
  (let ((file (ensure-readable-audio-file (bdef-file file))))
    (labels ((split (string &optional list)
               (let ((start (position #\newline string)))
                 (if start
                     (split (subseq string (1+ start)) (append list (list (subseq string 0 start))))
                     (append list (list string))))))
      (mapcar #'read-from-string ;; use "aubiocut -b FILE" to get a file cut by beats
              (split (uiop:run-program (list "python" (namestring (merge-pathnames *aubio-python-directory* "demo-tempo.py")) (namestring (truename file)))
                                       :output '(:string :stripped t)))))))

;;; bpm-tools

(defun bpm-tools-bpm (file)
  "Use bpm-tools' \"bpm\" utility to get the BPM of FILE."
  (values (read-from-string (uiop:run-program (format nil "sox -V1 \"~a\" -r 44100 -e float -c 1 -t raw - | bpm" file)
                                              :force-shell t :output '(:string :stripped t)))))

;;; audacity labels

(defun splits-from-audacity-labels (file)
  "Make a `splits' from an Audacity labels file."
  (with-open-file (stream file :direction :input :if-does-not-exist :error)
    (loop :for line := (read-line stream nil)
          :while line
          :for parsed := (string-split line :char-bag (list #\tab))
          :collect (parse-float:parse-float (car parsed)) :into starts
          :collect (parse-float:parse-float (cadr parsed)) :into ends
          :collect (caddr parsed) :into comments
          :finally (return (make-splits starts :ends ends :comments comments :unit :seconds)))))

(defmethod splits-export ((splits splits) (stream stream) (format (eql :audacity)))
  (loop :for idx :from 0 :below (length splits)
        :for start := (splits-point splits idx :start :second)
        :for end := (or (splits-point splits idx :end :second)
                        (ignore-errors (splits-point splits (1+ idx) :start :second)) ;; FIX: just use ensure-end, (or maybe integrate ensure-end into the regular splits-ends function?)
                        (end-point splits :second))
        :for comment := (splits-point splits idx :comment)
        :do (format stream "~a~c~a~c~a~%" start #\tab end #\tab (or comment idx))))

(uiop:with-deprecation (:style-warning)
  (defun audacity-labels-from-splits (splits &optional (file #P"~/label.txt"))
    "Deprecated alias for (splits-export SPLITS FILE :audacity)."
    (splits-export splits file :audacity)))

;;; op-1 drumsets
;; https://github.com/padenot/libop1/blob/master/src/op1_drum_impl.cpp

(defconstant +op-1-drumkit-end+ #x7FFFFFFE)

(defconstant +op-1-bytes-in-12-sec+ (* 44100 2 12))

(defconstant +size-of-uint16-t+ 2)

(defun frame-to-op-1-format (frame)
  "Convert a frame number to the OP-1 split point format.

See also: `op-1-format-to-frame'"
  (* (floor +op-1-drumkit-end+ +op-1-bytes-in-12-sec+)
     frame +size-of-uint16-t+))

(defun op-1-format-to-frame (op-1)
  "Convert a frame number to the OP-1 split point format.

See also: `op-1-format-to-frame'"
  (round
   (/  op-1
       (* (floor +op-1-drumkit-end+ +op-1-bytes-in-12-sec+)
          +size-of-uint16-t+))))

(defun splits-from-op-1-drumset (drumset &key (if-invalid :error))
  "Make a `splits' object by reading the metadata from an OP-1 format drumset file. IF-INVALID specifies what to do if DRUMSET can't be parsed as an op-1 drumset file; it can be either :ERROR to signal an error, or NIL to simply return nil."
  (assert (member if-invalid (list nil :error)) (if-invalid) "IF-INVALID must be either NIL or :ERROR.")
  (etypecase drumset
    (bdef
     (let ((splits (splits-from-op-1-drumset (bdef-file drumset))))
       (setf (splits-bdef splits) drumset)
       splits))
    (string
     (flet ((not-valid ()
              (cond ((null if-invalid)
                     (return-from splits-from-op-1-drumset nil))
                    ((eql :error if-invalid)
                     (error "Could not parse ~s as an op-1 drumset." drumset)))))
       (with-open-file (stream drumset :direction :input :element-type '(unsigned-byte 8) :if-does-not-exist :error)
         (loop :for peek := (read-byte stream nil nil)
               :if (null peek)
                 :do (not-valid)
               :if (= peek 111) ;; #\o
                 :do (let ((array (make-array 3 :initial-element nil)))
                       (read-sequence array stream)
                       (when (equalp #(112 45 49) array) ;; #\p #\- #\1
                         (let (accum)
                           (loop :do (push (read-byte stream nil nil) accum)
                                 :until (or (null (car accum))
                                            (eql 125 (car accum))))
                           (if (null (car accum))
                               (not-valid)
                               (return
                                 (let ((json (jsown:parse (coerce (mapcar #'code-char (nreverse accum)) 'string))))
                                   (make-splits (mapcar #'op-1-format-to-frame (jsown:val json "start"))
                                                :ends (mapcar #'op-1-format-to-frame (jsown:val json "end"))
                                                :unit :samples)))))))))))))

;;; snd marks
;; TODO

;;; echo nest / amen
;; https://github.com/algorithmic-music-exploration/amen
;; TODO

;;; acousticbrainz
;; https://beets.readthedocs.io/en/v1.4.7/plugins/acousticbrainz.html
;; TODO

