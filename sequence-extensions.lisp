(in-package #:bdef)

;;;; sequence-extensions.lisp
;; code to implement extensible sequences functionality.
;; https://github.com/guicho271828/common-lisp-extensions/issues/8
;; http://www.sbcl.org/manual/#Extensible-Sequences

;;; bdef

(defmethod sequence:length ((this bdef))
  (bdef-length this))

(defmethod sequence:emptyp ((this bdef))
  (= 0 (length this)))

(defmethod sequence:elt ((this bdef) index)
  (bdef-elt this index))

(defmethod sequence:subseq ((this bdef) start &optional end)
  (bdef-subseq this start end))

;;; splits

(defmethod sequence:length ((this splits))
  (splits-length this))

(defmethod sequence:emptyp ((this splits))
  (= 0 (length this)))

(defmethod sequence:elt ((this splits) index)
  (splits-point this index))

;; (defmethod (setf sequence:elt) (new-value (this splits) index)
;;   ;; (sequence:protocol-unimplemented 'sequence:elt this)
;;   ;; FIX
;;   )
