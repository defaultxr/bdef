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
   (bdef :initarg :bdef :accessor splits-bdef :type bdef :documentation "The bdef object that this splits references (i.e. for information like duration, sample rate, etc, for converting point data).")
   (metadata :initarg :metadata :initform (make-hash-table) :type hash-table :documentation "Hash table of additional data associated with the splits, accessible with the `splits-metadata' function."))
  (:documentation "List of split data for dividing buffers into pieces."))

(defmethod print-object ((splits splits) stream)
  (format stream "#<~A :LENGTH ~A :UNIT ~S~@[ :BDEF ~A~]>" 'splits
          (length (slot-value splits 'starts))
          (slot-value splits 'unit)
          (ignore-errors (splits-bdef splits))))

(defmethod describe-object ((splits splits) stream)
  (with-slots (starts ends loops comments unit bdef metadata) splits
    (format stream "~S is a ~S~@[ associated with ~S~].~%" splits 'splits (splits-bdef splits))
    (format stream "  It contains ~S splits (in ~(~A~)):~%" (splits-length splits) (splits-unit splits))
    (loop :with unit := (splits-unit splits)
          :for idx :from 0 :below (splits-length splits)
          :do (format stream "    ~S~@[ .. ~S~]~@[ (loop: ~S)~]~@[: ~S~]~%"
                      (splits-point splits idx :start unit)
                      (splits-point splits idx :end unit)
                      (splits-point splits idx :loop unit)
                      (splits-point splits idx :comment unit)))
    (let ((meta-keys (keys (splits-metadata splits))))
      (format stream "  It has ~S metadat~1:*~[a.~;um:~:;a:~]~%" (length meta-keys))
      (dolist (key meta-keys)
        (format stream "    ~S -> ~S~%" key (splits-metadata splits key))))))

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
  (check-type bdef (or null bdef))
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
                   :unit (splits-ensure-unit unit)
                   :bdef bdef
                   :metadata (etypecase metadata
                               (list (plist-hash-table metadata))
                               (hash-table metadata)))))

(defun splits-ensure-point-type (point)
  "Ensure POINT is one of the splits point types."
  (ecase (make-keyword point)
    ((:starts :start) :starts)
    ((:ends :end) :ends)
    ((:loops :loop) :loops)
    ((:comments :comment) :comments)))

(defun splits-ensure-unit (unit)
  "Ensure UNIT is one of the splits position unit types."
  (ecase (make-keyword unit)
    ((:percent :percents) :percents)
    ((:sample :samples :frame :frames) :samples)
    ((:second :seconds) :seconds)))

(defun splits-conversion-function-name (splits unit)
  "Get the name of the conversion function to convert SPLITS' unit type into the unit specified."
  (let ((unit (symbol-name (splits-ensure-unit unit)))
        (unit-slot (symbol-name (slot-value splits 'unit))))
    (if (string= unit unit-slot)
        'identity
        (intern (concat unit-slot "-" unit) 'bdef))))

(defun splits-conversion-function (splits unit)
  "Get a function that can be used to convert SPLITS' unit into the unit type specified."
  (let* ((unit (splits-ensure-unit unit))
         (conv-func (splits-conversion-function-name splits unit)))
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
  (check-type splits splits)
  (check-type point (member :start :end :loop :comment :starts :ends :loops :comments))
  (let ((array (slot-value splits (intern (symbol-name (splits-ensure-point-type point)) 'bdef)))
        (conv-func (splits-conversion-function splits (splits-ensure-unit unit))))
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
  (check-type split integer)
  (check-type point (member :start :end :loop :comment :starts :ends :loops :comments))
  (when-let* ((splits (if (typep splits '(or bdef symbol))
                          (bdef-splits splits)
                          splits))
              (unit (splits-ensure-unit unit))
              (array (slot-value splits (intern (symbol-name (splits-ensure-point-type point)) 'bdef)))
              (conv-func (splits-conversion-function splits unit)))
    (if (eql 'identity conv-func)
        (elt array split)
        (funcall conv-func (elt array split)))))

(defun end-point (object &optional (unit :percent))
  "Get the end point of OBJECT."
  (etypecase object
    ((or bdef splits)
     (ecase (make-keyword unit)
       ((:percent :percents) 1.0)
       ((:sample :frame :samples :frames) (bdef-length object))
       ((:second :seconds) (bdef-duration object))))))

(defun derive-split-end (splits split &key (unit :percents) (if-uncomputable :error))
  "Derive the end point of SPLIT in SPLITS for the specified UNIT. IF-COMPUTABLE defines what should be done if the split's end can't be derived; it can be one of these options:

- :error - Signal an error.
- :next-start - Return the next split's start point. Note that this might also end up signalling an error.
- nil - Return nil.

If SPLITS does not have end points, we derive the end point by assuming the end of one split is the start of the next. However, we can only do this for any split and any unit if we know the source buffer's length & rate. This is because the last split has no split after it to check the start of.

See also: `derive-split-dur', `splits-point'"
  (let ((splits (if (typep splits '(or bdef symbol))
                    (bdef-splits splits)
                    splits))
        (unit (splits-ensure-unit unit))
        (last-p (= split (1- (splits-length splits)))))
    (or (when (and (eql unit 'percents)
                   last-p)
          1)
        (when (and (splits-ends splits)
                   (eql unit (splits-unit splits)))
          (splits-point splits split :end unit))
        (when (or (and (splits-bdef splits)
                       (bdef-length splits)
                       (bdef-sample-rate splits))
                  (eql unit 'percents))
          (if last-p
              (end-point splits unit)
              (splits-point splits (1+ split) :start unit)))
        (when if-uncomputable
          (ecase if-uncomputable
            (:error (error "Could not derive the end of split ~S for ~S" split splits))
            (:next-start (splits-point splits (1+ split) :start unit)))))))

(defun derive-split-duration (splits start-split &key (end-split start-split) (if-uncomputable :error))
  "Derive the duration (in seconds) of the start of START-SPLIT to the end of END-SPLIT. If the duration can't be derived, error if IF-COMPUTABLE is :error, or just return nil if it is nil.

See also: `derive-split-dur', `derive-split-end', `splits-point'"
  (let ((end (derive-split-end splits end-split :unit :seconds :if-uncomputable if-uncomputable))
        (start (ignore-errors (splits-point splits start-split :start :seconds))))
    (when (and end start)
      (return-from derive-split-duration (abs (- end start))))
    (when if-uncomputable
      (ecase if-uncomputable
        (:error (error "Could not derive the duration of splits ~S..~S for ~S" start-split end-split splits))))))

(defun derive-split-dur (splits start-split &key (end-split start-split) (tempo 1) (if-uncomputable :error))
  "Derive the dur (in beats) of the start of START-SPLIT to the end of END-SPLIT. If the dur can't be derived, error if IF-COMPUTABLE is :error, or just return nil if it is nil.

See also: `derive-split-duration', `derive-split-end', `splits-point'"
  (if-let ((duration (derive-split-duration splits start-split :end-split end-split :if-uncomputable if-uncomputable)))
    (* duration tempo)
    (when if-uncomputable
      (ecase if-uncomputable
        (:error (error "Could not derive the dur of splits ~S..~S for ~S" start-split end-split splits))))))

(defgeneric splits-metadata (splits &optional key)
  (:documentation "Get the value of SPLITS's metadata for KEY, or the full metadata hashtable if KEY is not provided. Returns true as a second value if the metadata had an entry for KEY, or false if it did not."))

(defmethod splits-metadata ((splits splits) &optional key)
  (with-slots (metadata) splits
    (if key
        (gethash key metadata)
        metadata)))

(defmethod (setf splits-metadata) (value (splits splits) &optional key)
  (with-slots (metadata) splits
    (unless key
      (return-from splits-metadata (setf metadata (etypecase value
                                                    (list (plist-hash-table value))
                                                    (hash-table value)))))
    (when (typep value 'bdef)
      (unless (bdef-splits value)
        (setf (bdef-splits value) splits))
      (unless (slot-boundp splits 'bdef)
        (setf (splits-bdef splits) value)))
    (setf (gethash key metadata) value)))

(defmethod bdef-buffer ((this splits))
  (bdef-buffer (splits-bdef this)))

(defmethod bdef-length ((this splits))
  (or (splits-metadata this :length)
      (when-let ((bdef (splits-bdef this)))
        (bdef-length bdef))))

(defmethod bdef-sample-rate ((this splits))
  (or (splits-metadata this :sample-rate)
      (when-let ((bdef (splits-bdef this)))
        (bdef-sample-rate bdef))))

(defmethod bdef-channels ((this splits))
  (or (splits-metadata this :channels)
      (when-let ((bdef (splits-bdef this)))
        (bdef-channels bdef))))

(defmethod bdef-id ((this splits))
  (bdef-id (splits-bdef this)))

(defmethod bdef-file ((this splits))
  (bdef-file (splits-bdef this)))

(defmethod bdef-frames ((this splits) &key start end channels)
  (bdef-frames (splits-bdef this) :start start :end end :channels channels))

;;; splits import/export

(defun splits-import (source &optional format)
  "Derive a `splits' object from another format. If FORMAT is not provided, attempt to autodetect.

See also: `splits-export'"
  (splits-import-format source (or format
                                   (let ((detected (splits-import-detect-format source)))
                                     (unless detected
                                       (error "Could not detect a suitable import format for ~S." source))
                                     (if (length= 1 detected)
                                         (car detected)
                                         (error "Detected multiple possible import formats for ~S: ~S." source detected))))))

(defvar *splits-format-auto-detect-functions* nil
  "List of functions to check objects against when imported via `splits-import' without its format specified.")

(defun splits-import-detect-format (source)
  "Auto-detect the format of SOURCE by testing it through `*splits-format-auto-detect-functions*'. Returns a list of the possible formats detected."
  (remove nil (mapcar (fn (funcall _ source)) *splits-format-auto-detect-functions*)))

(defgeneric splits-import-format (input format)
  (:documentation "Make a `splits' object by importing from INPUT in FORMAT.

See also: `splits-import', `splits-export'"))

(defgeneric splits-export (splits destination format)
  (:documentation "Write a `splits' object as another format.

See also: `splits-import'"))

(defmethod splits-export ((bdef bdef) file format)
  (splits-export (bdef-splits bdef) file format))

(defmethod splits-export (object (file string) format)
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (splits-export object stream format)))

(defmethod splits-export (object (file pathname) format)
  (splits-export object (ensure-namestring file) format))

;;; splits import/export formats

;; filename / metadata

(defun string-extract-bpm (string &key (min-bpm 50) (max-bpm 400))
  "Extract the BPM from a string (typically a filename), or nil if no BPM-like string is found. If \"bpm\" is not in the string, then this function will look for a number in the range MIN-BPM to MAX-BPM, starting from the end of the string."
  (when (pathnamep string)
    (return-from string-extract-bpm (namestring string)))
  (check-type string string)
  (let* ((split (string-split string :char-bag (list #\space #\_ #\-)))
         (bpm-pos (position-if (lambda (s) (search "bpm" s :test #'char-equal)) split :from-end t)))
    (values (if bpm-pos
                (or (parse-float:parse-float (elt split bpm-pos) :junk-allowed t)
                    (parse-float:parse-float (elt split (1- bpm-pos)) :junk-allowed t))
                (loop :for i :in (nreverse split)
                      :for bpm := (parse-float:parse-float i :junk-allowed t)
                      :if (and (numberp bpm)
                               (<= min-bpm bpm max-bpm))
                        :return bpm)))))

;; aubio

(defvar *aubio-python-directory* "~/misc/aubio/python/demos/" ; FIX: can we do without this?
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
          (uiop:run-program (list (ecase utility
                                    (:onset "aubioonset")
                                    (:track "aubiotrack"))
                                  "--input" (ensure-readable-audio-file
                                             (ensure-namestring (if (bdef-p file)
                                                                    (bdef-file file)
                                                                    file)))
                                  "--bufsize" (write-to-string buf-size)
                                  "--hopsize" (write-to-string hop-size)
                                  "--onset" (string-downcase algorithm)
                                  "--onset-threshold" (write-to-string threshold)
                                  "-M" (write-to-string minimum-interval)
                                  "--silence" (write-to-string silence))
                            :output :lines)))

(defun splits-from-aubio (file &rest args &key &allow-other-keys)
  "Make a `splits' from onsets data generated by Aubio by using the `aubio-onsets' function.

See also: `splits', `aubio-onsets'"
  (apply #'make-splits
         (apply #'aubio-onsets file args)
         :unit :seconds
         (when (bdef-p file)
           (list :bdef file))))

(uiop:with-deprecation (:warning)
  (defun splits-from-aubio-onsets (file &rest args &key &allow-other-keys)
    "Deprecated alias for (splits-from-aubio FILE :utility :onset ...)."
    (apply #'splits-from-aubio file :utility :onset args)))

(defun aubio-demo-bpm (file &key (mode :default))
  "Use aubio's demo_bpm_extract.py to get the bpm of FILE."
  (check-type mode (member :default :fast :super-fast))
  (let* ((file (ensure-readable-audio-file (bdef-file file)))
         (string (uiop:run-program (list "python" (ensure-namestring (merge-pathnames *aubio-python-directory* "demo_bpm_extract.py")) "-m" (string-downcase mode) (ensure-namestring file))
                                   :output '(:string :stripped t))))
    (car (multiple-value-list (read-from-string (subseq string 0 (position #\space string)))))))

(defun aubio-demo-tempo (file &key sample-rate)
  "Use aubio's demo_tempo.py to get a list of beats in FILE. The returned list gives onsets in seconds from the start of the file."
  (declare (ignore sample-rate)) ; FIX: actually use the sample-rate key
  ;; FIX: uiop:run-program allows you to read output as a stream. that might be better?
  (let ((file (ensure-readable-audio-file (bdef-file file))))
    (labels ((split (string &optional list)
               (let ((start (position #\newline string)))
                 (if start
                     (split (subseq string (1+ start)) (append list (list (subseq string 0 start))))
                     (append list (list string))))))
      (mapcar #'read-from-string ; use "aubiocut -b FILE" to get a file cut by beats
              (split (uiop:run-program (list "python" (namestring (merge-pathnames *aubio-python-directory* "demo-tempo.py")) (ensure-namestring file))
                                       :output '(:string :stripped t)))))))

;; bpm-tools
;; https://www.pogo.org.uk/~mark/bpm-tools/
;; FIX: check if "bpm" actually exists
;; FIX: remove use of "sox"?
;; FIX: don't use :force-shell t

(defun bpm-tools-bpm (file)
  "Use bpm-tools' \"bpm\" utility to get the BPM of FILE."
  (values (read-from-string (uiop:run-program (format nil "sox -V1 \"~A\" -r 44100 -e float -c 1 -t raw - | bpm" file)
                                              :force-shell t :output '(:string :stripped t)))))

;; audacity labels

(defun detect-audacity-labels (file)
  "Returns :audacity if FILE is a valid Audacity labels file."
  (check-type file (or pathname-designator stream))
  (when (pathname-designator-p file)
    (with-open-file (stream (ensure-namestring file) :direction :input :if-does-not-exist :error)
      (return-from detect-audacity-labels (detect-audacity-labels stream))))
  (loop :for line := (read-line file nil)
        :while line
        :for split := (string-split line :count 3 :char-bag (list #\tab))
        :if (and (not (emptyp line))
                 (or (not (length= 3 split))
                     (position-if-not (fn (ignore-errors (parse-float:parse-float _)))
                                      (subseq split 0 2))))
          :do (return nil)
        :finally (return :audacity)))

(pushnew 'detect-audacity-labels *splits-format-auto-detect-functions*)

(defmethod splits-import-format ((path string) (format (eql :audacity)))
  (with-open-file (stream (ensure-namestring path) :direction :input :if-does-not-exist :error)
    (splits-import-format stream :audacity)))

(defmethod splits-import-format ((stream stream) (format (eql :audacity)))
  (loop :for line := (read-line stream nil)
        :while line
        :for parsed := (string-split line :char-bag (list #\tab))
        :collect (parse-float:parse-float (car parsed)) :into starts
        :collect (parse-float:parse-float (cadr parsed)) :into ends
        :collect (caddr parsed) :into comments
        :finally (return (make-splits starts :ends ends :comments comments :unit :seconds))))

(uiop:with-deprecation (:style-warning)
  (defun splits-from-audacity-labels (file)
    "Deprecated alias for (splits-import-format FILE :audacity).

See also: `splits-import', `splits-import-format'."
    (splits-import-format file :audacity)))

(defmethod splits-export ((splits splits) (stream stream) (format (eql :audacity)))
  (loop :for idx :from 0 :below (length splits)
        :for start := (splits-point splits idx :start :second)
        :for end := (or (splits-point splits idx :end :second)
                        (ignore-errors (splits-point splits (1+ idx) :start :second)) ; FIX: just use ensure-end, (or maybe integrate ensure-end into the regular splits-ends function?)
                        (end-point splits :second))
        :for comment := (splits-point splits idx :comment)
        :do (format stream "~A~C~A~C~A~%" start #\tab end #\tab (or comment idx))))

(uiop:with-deprecation (:warning)
  (defun audacity-labels-from-splits (splits &optional (file #P"~/label.txt"))
    "Deprecated alias for (splits-export SPLITS FILE :audacity)."
    (splits-export splits file :audacity)))

;; op-1 drumsets
;; https://github.com/padenot/libop1/blob/master/src/op1_drum_impl.cpp

(defconstant +op-1-drumkit-end+ #x7FFFFFFE)

(defconstant +op-1-bytes-in-12-sec+ (* 44100 2 12))

(defconstant +size-of-uint16-t+ 2)

(defun frame-to-op-1-format (frame)
  "Convert a frame number to the OP-1 split point format.

See also: `op-1-format-to-frame'"
  (* (floor +op-1-drumkit-end+ +op-1-bytes-in-12-sec+) frame +size-of-uint16-t+))

(defun op-1-format-to-frame (op-1)
  "Convert a frame number to the OP-1 split point format.

See also: `op-1-format-to-frame'"
  (round (/ op-1 (* (floor +op-1-drumkit-end+ +op-1-bytes-in-12-sec+)
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
                     (error "Could not parse ~S as an op-1 drumset." drumset)))))
       (with-open-file (stream drumset :direction :input :element-type '(unsigned-byte 8) :if-does-not-exist :error)
         (loop :for peek := (read-byte stream nil nil)
               :unless peek
                 :do (not-valid)
               :when (= peek 111) ; #\o
                 :do (let ((array (make-array 3 :initial-element nil)))
                       (read-sequence array stream)
                       (when (equalp #(112 45 49) array) ; #\p #\- #\1
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

;; snd marks
;; TODO

;; echo nest / amen
;; https://github.com/algorithmic-music-exploration/amen
;; TODO

;; acousticbrainz
;; https://beets.readthedocs.io/en/v1.4.7/plugins/acousticbrainz.html
;; TODO

