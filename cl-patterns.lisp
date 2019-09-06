(in-package #:bdef)

(defmethod cl-patterns:play ((bdef bdef))
  (cl-patterns:play (cl-patterns:event :instrument :sp :bufnum bdef :latency 0 :quant 0)))

(defmethod cl-patterns::supercollider-convert-object ((object bdef) key)
  (declare (ignore key))
  (sc:bufnum (slot-value object 'buffer)))

(defmethod cl-patterns::supercollider-convert-object ((object symbol) key)
  (declare (ignore key))
  (let ((bdef (bdef-get object)))
    (if bdef
        (sc:bufnum (slot-value bdef 'buffer))
        object)))

(defmethod cl-patterns::tempo ((bdef bdef))
  (bdef-metadata bdef :tempo))

(defmethod cl-patterns::dur ((bdef bdef))
  (* (duration bdef) (tempo bdef)))
