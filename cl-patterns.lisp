(in-package #:cl-patterns)

(affixnew *dictionary-lookup-functions* 'bdef:bdef)

;;; generics

(defmethod backend-convert-object ((bdef bdef:bdef) key (backend (eql :supercollider)))
  (declare (ignore key))
  (bdef:bdef-id (bdef:bdef-buffer bdef)))

(defmethod backend-convert-object ((bdef bdef:bdef) key (backend (eql :incudine)))
  (declare (ignore key))
  (bdef:bdef-buffer bdef))

(defmethod backend-convert-object ((object symbol) key (backend (eql :supercollider)))
  (declare (ignore key))
  (let ((bdef (bdef::bdef-ref object)))
    (if bdef
        (bdef:bdef-id (bdef:bdef-buffer bdef))
        object)))

(defmethod backend-convert-object ((object symbol) key (backend (eql :incudine)))
  (declare (ignore key))
  (let ((bdef (bdef::bdef-ref object)))
    (if bdef
        (bdef:bdef-buffer bdef)
        object)))

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

(defun splits-event (splits split &key end-split (unit :percent))
  "Get an `event' for a `splits' split."
  (flet ((ensure-end (end-split unit)
           (or (splits-point splits end-split :end unit)
               (let ((ns (1+ end-split)))
                 (when (< ns (splits-length splits))
                   (splits-point splits (1+ end-split) :start unit)))
               (end-point splits unit))))
    (let* ((end-split (or end-split split))
           (start (splits-point splits split :start unit))
           (end (ensure-end end-split unit)))
      (cl-patterns:event :start start :end end
             :dur (cl-patterns:time-dur (abs (- (ensure-end end-split :seconds)
                                    (splits-point splits split :start :seconds)))
                            (cl-patterns:event-value cl-patterns:*event* :tempo))))))

(export '(splits-event))

(in-package #:cl-patterns)

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
                                    (bdef:bdef-splits (bdef::bdef-ref buffer)))
                                   (t
                                    nil))))))
               (split (if split
                          (next split)
                          (event-value *event* :split))))
      (unless (>= split (bdef:splits-length splits))
        (bdef::splits-event splits split)))))

(export '(psplits psplits-pstream))
