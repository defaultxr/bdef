(in-package #:cl-patterns)

(unless (member 'bdef:find-bdef *dictionary-lookup-functions*)
  (appendf *dictionary-lookup-functions* (list 'bdef:find-bdef)))

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

(defmethod duration ((bdef bdef:bdef))
  (bdef:bdef-duration bdef))

;; FIX: these methods might not work for non-SC backends??
(defmethod render ((list list) (output (eql :bdef)) &rest args &key &allow-other-keys)
  (bdef:bdef (apply #'render list (generate-temporary-file-name :directory bdef:*bdef-temporary-directory*
                                                                :extension :wav)
                    args)))

(defmethod render ((pattern pattern) (output (eql :bdef)) &rest args &key &allow-other-keys)
  (apply #'render (as-supercollider-score pattern) :bdef args)) ; FIX: this should probably just call another `render' rather than `as-supercollider-score'

(defmethod render ((bdef bdef:bdef) output &rest args &key &allow-other-keys)
  (apply #'render (bdef:bdef-buffer bdef) output args))

;;; splits

(in-package #:bdef)

(defun splits-event (splits split &key end-split (unit :percents))
  "Get a `cl-patterns:event' for a `splits' split. The resulting event will always have at least a :start key, but may also have:

- :end - if END-SPLIT is provided or can be derived (`derive-split-end')
- :dur - if the dur can be derived (`derive-split-dur')

See also: `splits-events'"
  (let* ((start (splits-point splits split :start unit))
         (end-split (or end-split split))
         (end (derive-split-end splits end-split :unit unit :if-uncomputable nil))
         (bdef (splits-bdef splits))
         (tempo (or (and bdef (bdef-metadata bdef :tempo))
                    (cl-patterns:event-value cl-patterns:*event* :tempo)
                    (cl-patterns:tempo cl-patterns:*clock*)))
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
  (loop :with remove-keys := (ensure-list remove-keys)
        :with beat := 0
        :for idx :below (splits-length splits)
        :for ev := (splits-event splits idx)
        :for r-ev := (cl-patterns:combine-events ev (cl-patterns:event :beat beat))
        :do (dolist (remove-key (append (set-difference (keys r-ev) keys) remove-keys))
              (cl-patterns:remove-event-value r-ev remove-key))
        :collect r-ev
        :do (incf beat (cl-patterns:event-value ev :delta))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(splits-event splits-events)))

(in-package #:cl-patterns)

(defpattern psplits ()
  ((splits :initform nil)
   (split :initform nil))
  :documentation "Yields events for split information based on the buffer of the current `*event*'."
  :defun (defun psplits (&optional splits split &key unit)
           (make-instance 'psplits :splits splits
                                   :split split)))

;; FIX: define behaviors for when the selected split is out of range (error, clip, wrap, fold, etc)
(defmethod as-pstream ((psplits psplits))
  (with-slots (splits split) psplits
    (make-instance 'psplits-pstream :splits (pattern-as-pstream splits)
                                    :split (pattern-as-pstream split))))

(defmethod next ((psplits psplits-pstream)) ; FIX: implement UNIT
  (with-slots (splits split) psplits
    (let ((splits (if splits
                      (next splits)
                      (or (event-value *event* :splits)
                          (let ((buffer (or (event-value *event* :buffer) (event-value *event* :bufnum))))
                            (typecase buffer
                              (null eop)
                              (bdef:bdef (bdef:bdef-splits buffer))
                              (symbol (bdef:bdef-splits (bdef:find-bdef buffer)))
                              (t eop))))))
          (split (if split
                     (next split)
                     (event-value *event* :split))))
      (when (or (eop-p splits)
                (eop-p split))
        (return-from next eop))
      (if (>= split (bdef:splits-length splits))
          eop
          (bdef:splits-event splits split)))))

(export '(psplits psplits-pstream))
