(in-package #:cl-patterns)

;;; generics

(defmethod play ((bdef bdef:bdef))
  (play (bdef:bdef-buffer bdef)))

(defmethod supercollider-convert-object ((bdef bdef:bdef) key)
  (declare (ignore key))
  (bdef::id (bdef:bdef-buffer bdef)))

(defmethod supercollider-convert-object ((object symbol) key)
  (declare (ignore key))
  (let ((bdef (bdef::bdef-get object)))
    (if bdef
        (bdef::id (bdef:bdef-buffer bdef))
        object)))

(defmethod tempo ((bdef bdef:bdef))
  (bdef:bdef-metadata bdef :tempo))

(defmethod dur ((bdef bdef:bdef))
  (* (bdef::duration bdef) (tempo bdef)))

;;; splits

(defun bdef::splits-event (splits split &key end-split (type :percent))
  "Get an `event' for a `bdef:splits' split."
  (flet ((ensure-end (end-split type)
           (or (bdef::splits-point splits end-split :end type)
               (let ((ns (1+ end-split)))
                 (when (< ns (bdef::splits-length splits))
                   (bdef::splits-point splits (1+ end-split) :start type)))
               (bdef::end-point splits type))))
    (let* ((end-split (or end-split split))
           (start (bdef::splits-point splits split :start type))
           (end (ensure-end end-split type)))
      (event :start start :end end
             :dur (time-dur (abs (- (ensure-end end-split :seconds)
                                    (bdef::splits-point splits split :start :seconds)))
                            (event-value *event* :tempo))))))

(defpattern psplits ()
  ((splits :default nil)
   (split :default nil))
  :documentation "Yields events for split information based on the :bufnum of the current `*event*'.")

;; FIX: define behaviors for when the selected split is out of range (error, clip, wrap, fold, etc)
(defmethod as-pstream ((psplits psplits))
  (with-slots (splits split) psplits
    (make-instance 'psplits-pstream
                   :splits (pattern-as-pstream splits)
                   :split (pattern-as-pstream split))))

(defmethod next ((psplits psplits-pstream))
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
                                    (bdef:bdef-splits (bdef::bdef-get buffer)))
                                   (t
                                    nil))))))
               (split (if split
                          (next split)
                          (event-value *event* :split))))
      (unless (>= split (bdef:splits-length splits))
        (bdef::splits-event splits split)))))
