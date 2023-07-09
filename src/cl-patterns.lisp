(in-package #:cl-patterns)

(affixnew *dictionary-lookup-functions* 'bdef:bdef)

;;; methods

(defmethod backend-convert-object ((bdef bdef:bdef) key backend)
  (declare (ignore key backend))
  (bdef:bdef-id (bdef:bdef-buffer bdef)))

(defmethod play ((bdef bdef:bdef))
  (play (bdef:bdef-buffer bdef)))

(defmethod tempo ((bdef bdef:bdef))
  (bdef:bdef-tempo bdef))

(defmethod dur ((bdef bdef:bdef))
  (bdef:bdef-dur bdef))

;; FIX: these methods might not work for non-SC backends??
(defmethod render ((list list) (output (eql :bdef)) &rest args &key &allow-other-keys)
  (bdef:bdef (apply #'render list (generate-temporary-file-name :directory bdef:*bdef-temporary-directory*
                                                                :extension :wav)
                    args)))

(defmethod render ((pattern pattern) (output (eql :bdef)) &rest args &key &allow-other-keys)
  (apply #'render (as-score pattern) :bdef args))

(defmethod render ((bdef bdef:bdef) output &rest args &key &allow-other-keys)
  (apply #'render (bdef:bdef-buffer bdef) output args))

;;; splits

(in-package #:bdef)

(defun derive-split-end (splits split &key (unit :percents) (if-uncomputable :error)) ; FIX: move outside of cl-patterns.lisp
  "Derive the end point of SPLIT in SPLITS for the specified UNIT. If the end point can't be derived, error if IF-COMPUTABLE is :error, or just return nil if it is nil.

If SPLITS does not have end points, we derive the end point by assuming the end of one split is the start of the next. However, we can only do this for any split and any unit if we know the source buffer's length & rate. This is because the last split has no split after it to check the start of.

See also: `derive-split-dur', `splits-point'"
  (let ((splits (if (typep splits '(or bdef symbol))
                    (bdef-splits splits)
                    splits))
        (unit (%splits-ensure-unit unit))
        (last-p (= split (1- (splits-length splits)))))
    (or (when (and (eql unit 'percents)
                   last-p)
          1)
        (when (splits-ends splits)
          (splits-point splits split :end unit))
        (when (or (and (splits-bdef splits)
                       (bdef-length splits)
                       (bdef-sample-rate splits))
                  (eql unit 'percents))
          (if last-p
              (end-point splits unit)
              (splits-point splits (1+ split) :start unit)))
        (when (eql :error if-uncomputable)
          (error "Could not derive the end of split ~s for ~s" split splits)))))

(defun derive-split-duration (splits start-split &key end-split (if-uncomputable :error)) ; FIX: move outside of cl-patterns.lisp
  "Derive the duration in seconds of the start of START-SPLIT to the end of END-SPLIT. If the duration can't be derived, error if IF-COMPUTABLE is :error, or just return nil if it is nil. If END-SPLIT is nil or not provided, ; FIX

See also: `derive-split-dur', `derive-split-end', `splits-point'"
  (let ((end (derive-split-end splits (or end-split start-split) :unit :seconds :if-uncomputable if-uncomputable))
        (start (splits-point splits start-split :start :seconds)))
    (abs (- end start))))

(defun derive-split-dur (splits start-split &key end-split (tempo 1) (if-uncomputable :error))
  "Derive the duration in beats of the start of START-SPLIT to the end of END-SPLIT. If the dur can't be derived, error if IF-COMPUTABLE is :error, or just return nil if it is nil. If ; FIX

See also: `derive-split-duration', `derive-split-end', `splits-point'"
  (cl-patterns:time-dur
   (derive-split-duration splits start-split :end-split end-split :if-uncomputable if-uncomputable)
   tempo))

(defun splits-event (splits split &key end-split (unit :percents))
  "Get an `event' for a `splits' split. The event will always have at least a start key, but will also have:

- end key if END-SPLIT is provided or derivable (`derive-split-end')
- dur key if

See also: `splits-events'" ; FIX
  (let* ((start (splits-point splits split :start unit))
         (end-split (or end-split split))
         (end (derive-split-end splits end-split :unit unit :if-uncomputable nil))
         (tempo (or (cl-patterns:e :tempo) (cl-patterns:tempo cl-patterns:*clock*))) ; FIX: check bdef's tempo instead?
         (dur (derive-split-dur splits split :end-split end-split :tempo tempo :if-uncomputable nil)))
    (apply #'cl-patterns:event
           :start start
           (append (when end
                     (list :end end))
                   (when dur
                     (list :dur dur))))))

(defun splits-events (splits &key (unit :percents) (keys (list :start :end :dur :beat)) remove-keys)
  "Convert SPLITS into a list of `cl-patterns:event's.

See also: `splits-event', `bdef-splits'"
  (unless (eql unit :percents) ; FIX: implement it
    (error "~S's ~S argument is not yet implemented" 'splits-events 'unit))
  (let ((beat 0))
    (loop :for idx :below (splits-length splits)
          :for ev := (splits-event splits idx)
          :for r-ev := (cl-patterns:combine-events ev (cl-patterns:event :beat beat))
          :do (dolist (remove-key (append (set-difference (keys r-ev) keys) remove-keys))
                (cl-patterns:remove-event-value r-ev remove-key))
          :collect r-ev
          :do (incf beat (cl-patterns:event-value ev :delta)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(splits-event splits-events)))

(in-package #:cl-patterns)

(defpattern psplits ()
  ((splits :initform nil)
   (split :initform nil))
  :documentation "Yields events for split information based on the buffer of the current `*event*'.")

;; FIX: define behaviors for when the selected split is out of range (error, clip, wrap, fold, etc)
(defmethod as-pstream ((psplits psplits))
  (with-slots (splits split) psplits
    (make-instance 'psplits-pstream
                   :splits (pattern-as-pstream splits)
                   :split (pattern-as-pstream split))))

(defmethod next ((psplits psplits-pstream)) ; FIX: implement UNIT
  (with-slots (splits split) psplits
    (when-let ((splits (if splits
                           (next splits)
                           (or (event-value *event* :splits)
                               (when-let ((buffer (or (event-value *event* :buffer)
                                                      (event-value *event* :bufnum))))
                                 (typecase buffer
                                   (bdef::bdef
                                    (bdef:bdef-splits buffer))
                                   (symbol
                                    (bdef:bdef-splits (bdef:find-bdef buffer)))
                                   (t
                                    nil))))))
               (split (if split
                          (next split)
                          (event-value *event* :split))))
      (unless (>= split (bdef:splits-length splits))
        (bdef::splits-event splits split)))))

(export '(psplits psplits-pstream))
