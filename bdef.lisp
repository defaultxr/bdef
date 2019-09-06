;;;; bdef.lisp

(in-package #:bdef)

;;; global variables/configuration

(defparameter *bdef-temporary-directory*
  #+linux "/tmp/supercollider/"
  #+darwin "/tmp/supercollider/"
  #+windows (concatenate 'string (uiop:getenv-pathname "USERPROFILE") "/AppData/Local/")
  "The directory bdef should store its temporary files in (i.e. the .wav files generated from format auto-conversion).")

(defparameter *ffmpeg-path* ;; FIX: actually use this...
  #+(or linux darwin) (ignore-errors (uiop:run-program "which ffmpeg"))
  #+windows (uiop:getenv-pathname "TEMP")
  "The path to ffmpeg, or nil if ffmpeg could not be found.")

;;; generics

(defgeneric path (object)
  (:documentation "Get the path of OBJECT (i.e. if a buffer, typically the path to the file that the buffer was loaded from.)"))

(defmethod path ((bdef bdef))
  (path (bdef-buffer bdef)))

(defmethod path ((string string))
  string)

(defmethod path ((pathname pathname))
  pathname)

(defgeneric duration (object)
  (:documentation "Get the duration of OBJECT in seconds. Differs from `dur' in that dur is used to get the number of beats.

See also: `dur'"))

(defmethod duration ((bdef bdef))
  (duration (bdef-buffer bdef)))

;; (export '(duration)) ;; FIX: this gives a symbol conflict for some reason???

(defgeneric frames (object)
  (:documentation "Get the number of frames (samples) in OBJECT."))

(defmethod frames ((bdef bdef))
  (frames (bdef-buffer bdef)))

(defgeneric num-channels (object)
  (:documentation "Get the number of channels of OBJECT."))

(defmethod num-channels ((bdef bdef))
  (num-channels (bdef-buffer bdef)))

(defgeneric path (object)
  (:documentation "Get the path to the file of OBJECT, or nil if it was not loaded from a path."))

(defmethod path ((bdef bdef))
  (path (bdef-buffer bdef)))

(defmethod path ((symbol symbol))
  (path (bdef-buffer (bdef symbol))))

;;; file handling

(defun ensure-readable-audio-file (path &key (extensions (list "wav" "aif" "aiff")))
  "If PATH ends in any of EXTENSIONS, return it unchanged. Otherwise, use ffmpeg to convert it to wav."
  (let* ((path (namestring (truename path)))
         (ext-pos (position #\. path :from-end t)))
    (if (position (string-downcase (subseq path (1+ ext-pos))) extensions :test #'equal)
        path
        (let ((output-filename (concatenate 'string *bdef-temporary-directory* (file-namestring (subseq path 0 ext-pos)) ".wav")))
          (if (probe-file output-filename)
              output-filename
              (progn
                (ensure-directories-exist *bdef-temporary-directory*)
                (uiop:run-program (list "ffmpeg" "-i" path output-filename) ;; FIX: cache this output and parse it for bdef metadata
                                  ;; :output '(:string :stripped t))
                                  )
                output-filename))))))

;; FIX: consider using easy-audio to get audio file metadata instead?
(defun ffmpeg-data (file)
  "Get the ffmpeg output for FILE.."
  (nth-value 1 (uiop:run-program (list "ffmpeg" "-i" (namestring (truename file))) :error-output '(:string :stripped t) :ignore-error-status t)))

(defun ffmpeg-metadata (file)
  "Get the metadata for a file from ffmpeg's output."
  (alexandria:when-let* ((split (split-sequence:split-sequence #\newline (ffmpeg-data file)))
                         (metadata-pos (position "  Metadata:" split :test 'string=))
                         (metadata-end (position-if (lambda (s) (eq 0 (search "  Duration: " s))) split))
                         (metadata (subseq split (1+ metadata-pos) metadata-end)))
    (loop :for i :in metadata
       :for pos = (position #\: i)
       :for key = (subseq i 0 pos)
       :for value = (subseq i (1+ pos))
       :append (list (intern (string-upcase (string-trim (list #\space) key)) :keyword)
                     (string-trim (list #\space) value)))))

(defun buffer-read-any (path &key (num-channels 2) wavetable (start-frame 0) bufnum (server sc:*s*))
  "Read any file as a buffer, making sure it's converted to a SuperCollider-compatible format first, and making sure it loads as a stereo buffer even if the file is mono."
  (let* ((path (ensure-readable-audio-file (namestring (truename path))))
         (ffmpeg-data (ffmpeg-data path))
         (stream-line-pos (search "Stream #0:0: Audio: " ffmpeg-data))
         (newline (position #\newline ffmpeg-data :start stream-line-pos))
         (mono-p (search "mono," (subseq ffmpeg-data stream-line-pos newline))))
    (when (/= 0 start-frame)
      (warn "Loading sounds with a start frame is not yet supported.")) ;; FIX
    (if wavetable
        (cl-collider:buffer-read-as-wavetable path)
        (cl-collider:buffer-read-channel path
                                         :channels (cond ((null num-channels)
                                                          -1)
                                                         ((= num-channels 1)
                                                          (if mono-p
                                                              (list 0)
                                                              (progn
                                                                (warn "The requested sound (\"~s\") is stereo but a 1-channel buffer was requested. Mixing stereo files down to mono is not yet supported; therefore only the first channel was loaded." path)
                                                                (list 0))))
                                                         ((= num-channels 2)
                                                          (if mono-p
                                                              (list 0 0)
                                                              (list 0 1))))
                                         :bufnum bufnum
                                         :server server))))

;;; wavetable shiz
;; https://github.com/supercollider/supercollider/blob/772bdd6946a7253390ba0b1b23eb3adc0acb97ea/common/Samp.cpp#L29

;; (defparameter buf (buffer-read #P"~/misc/sounds/ssw/inspektor/Single Cycle in C/AKWF_0001/AKWF_0001.wav"))

;; (defparameter buf (buffer-read-as-wavetable #P"~/misc/sounds/ssw/inspektor/Single Cycle in C/AKWF_0001/AKWF_0001.wav"))

;; | server=(Server.default) path action numChannels=1 size |
;; var sf, fa, po2;
;; sf = SoundFile.new;
;; sf.openRead(path);
;; fa = FloatArray.newClear(sf.numFrames);
;; sf.readData(fa);
;; sf.close;
;; po2 = (2**(1..15));
;; size = (size ? sf.numFrames);
;; if(po2.includes(size).not, {
;; 	var index = (po2.indexOfGreaterThan(size) ? 10);
;; 	"Rounding wavetable buffer size to the nearest power of 2.".warn;
;; 	size = po2[index.clip(0, 14)];
;; });
;; if(size != sf.numFrames, {
;; 	fa = fa.resamp1(size);
;; });
;; fa = fa.as(Signal).asWavetable;
;; ^Buffer.loadCollection(server, fa, numChannels ? 1, action);

;; (defun buffer-read-as-wavetable (path)
;;   (let* ((tmp-buf (prog1 (buffer-read path)
;;                     (sync)))
;;          (file-frames (slot-value tmp-buf 'sc::frames))
;;          (powers-of-two (mapcar (lambda (x) (expt 2 (1+ x))) (alexandria:iota 16)))
;;          (num-frames (nth (position-if (lambda (x) (>= x file-frames)) powers-of-two) powers-of-two))
;;          (frames (prog1
;;                      (buffer-get-to-list tmp-buf)
;;                    (buffer-free tmp-buf)))
;;          (buffer (buffer-alloc (* 2 num-frames))))
;;     (buffer-set-list buffer (list-in-wavetable-format (linear-resample frames num-frames)))
;;     buffer))

;; (defun buffer-get-range (buffer &optional (start 0) (end (slot-value buffer 'sc::frames)))
;;   "Get a list of the frames of BUFFER. Unlike `buffer-get-list', this function is not limited by OSC packet size and can return any number of frames, though it may be slower."
;;   (loop :while (< start end)
;;      :append
;;        (let ((dec (min 400 (- end start))))
;;          (prog1
;;              (buffer-get-list buffer start dec)
;;            (incf start dec)))))

;; see http://doc.sccode.org/Classes/Wavetable.html#Advanced%20notes:%20wavetable%20format
;; (defun list-in-wavetable-format (list)
;;   "Convert a list of numbers LIST to SuperCollider's wavetable format."
;;   (loop :for i :from 0 :below (length list)
;;      :append (let ((a0 (nth-wrap i list))
;;                    (a1 (nth-wrap (1+ i) list)))
;;                (list (- (* 2 a0) a1) (- a1 a0)))))

;; (defun nth-wrap (n list)
;;   "Return the Nth value of LIST, wrapping around if the value is bigger or smaller than the list length."
;;   (nth (mod n (length list)) list))

;; (defun blend-nth (n list)
;;   "Get the Nth value of LIST, linearly interpolating between the adjacent values if N is not an integer."
;;   (if (= n (round n))
;;       (nth n list)
;;       (let* ((floor (floor n))
;;              (ceiling (ceiling n))
;;              (fl-diff (- n floor)))
;;         (+ (* (nth floor list) (- 1 fl-diff))
;;            (* (nth ceiling list) fl-diff)))))

;; (defun linear-resample (list new-size)
;;   "Using linear interpolation, resample the values of LIST to a new list of size NEW-SIZE."
;;   (let* ((old-size (length list))
;;          ;; this.size - 1 / (newSize - 1).max(1);
;;          (factor (/ (1- old-size) (max (1- new-size) 1))))
;;     (if (= old-size new-size)
;;         list
;;         (loop :for i :from 0 :below new-size
;;            :collect (blend-nth (* i factor) list)))))

;;; bdef

(defclass bdef ()
  ((key :initarg :key :reader bdef-key)
   (buffer :initarg :buffer :reader bdef-buffer)
   (metadata :initarg :metadata)))

(defmethod print-object ((bdef bdef) stream)
  (with-slots (key buffer metadata) bdef
    (if (slot-boundp bdef 'key)
        (format stream "(~s ~s)" 'bdef key)
        (format stream "#<~s>" 'bdef))))

(defmethod describe-object ((bdef bdef) stream) ;; FIX
  (with-slots (key buffer metadata) bdef
    (format stream "~s is a ~s.~%  It is ~s seconds long (~s frames), with ~s channels.~%" bdef 'bdef (duration bdef) (frames bdef) (num-channels bdef))
    (format stream "~@[  It contains the audio from the file ~s.~%~]" (path bdef))
    (format stream "  Keys that point to this buffer are: ~s~%" (bdef-keys-pointing-to bdef))
    (alexandria:when-let ((meta (bdef-metadata bdef)))
      (format stream "  It has the following metadata:~%")
      (loop :for (key val) :on meta :by #'cddr
         :do (format stream "    ~s -> ~s~%" key val)))))

(defvar *bdef-dictionary* (make-hash-table :test #'equal)
  "The global dictionary of bdefs.")

(defun all-bdefs (&optional include-aliases)
  "Return a list of the names of all bdefs loaded.

Note that this doesn't include aliases (i.e. bdef keys that point to another key) unless INCLUDE-ALIASES is true."
  (if include-aliases
      (alexandria:hash-table-keys *bdef-dictionary*)
      (loop :for i :being :the hash-keys :of *bdef-dictionary*
         :if (typep (gethash i *bdef-dictionary*) 'bdef)
         :collect i)))

(defun bdef-dictionary-keys (&key (include-redirects t) (dictionary *bdef-dictionary*))
  "Return a list of all the keys for the bdef dictionary DICTIONARY."
  (let ((keys (alexandria:hash-table-keys dictionary)))
    (if include-redirects
        keys
        (loop :for key :in keys
           :if (typep (gethash key dictionary) 'bdef)
           :collect key))))

(defun bdef-key-cleanse (key)
  "Expands pathnames to their full ones."
  (typecase key
    (pathname (namestring (truename key)))
    (string (bdef-key-cleanse (pathname key)))
    (otherwise key)))

(defun ensure-bdef (object) ;; FIX: use this in all bdef functions
  "Return OBJECT if object is a bdef, otherwise look up a bdef with OBJECT as its key."
  (if (typep object 'bdef)
      object
      (bdef object)))

(defun bdef-get (key &optional (dictionary *bdef-dictionary*))
  "Get the value of KEY in the bdef dictionary DICTIONARY."
  (let* ((key (bdef-key-cleanse key))
         (result (gethash key dictionary)))
    (if (and (not (null result))
             (typep result '(or string symbol)))
        (bdef-get result dictionary)
        result)))

(defun bdef-metadata (bdef &optional key)
  "Get BDEF's metadata for KEY."
  (let ((bdef (bdef bdef)))
    (if key
        (getf (slot-value bdef 'metadata) key)
        (slot-value bdef 'metadata))))

(defsetf bdef-metadata (bdef &optional (key nil key-provided-p)) (value) ;; FIX: automatically set any splits objects' BDEF slot to point to this bdef.
  (let ((bdef-sym (gensym))
        (plist-sym (gensym))
        (key-sym (gensym))
        (value-sym (gensym)))
    `(let ((,bdef-sym ,bdef)
           (,value-sym ,value))
       (if ,key-provided-p
           (let ((,plist-sym (bdef-metadata ,bdef-sym))
                 (,key-sym ,key))
             (when (and (typep ,value-sym 'splits)
                        (null (splits-bdef ,value-sym)))
               (setf (splits-bdef ,value-sym) ,bdef-sym))
             (setf (slot-value ,bdef-sym 'metadata) (cl-patterns::plist-set ,plist-sym ,key-sym ,value-sym))
             ,value-sym)
           (progn
             (setf (slot-value ,bdef-sym 'metadata) ,value-sym)
             ,value-sym)))))

(defun bdef-splits (bdef)
  "Get any `splits' from BDEF's metadata, searching in preferred order (i.e. :splits key first, etc)."
  (or (bdef-metadata bdef :splits)
      (bdef-metadata bdef :onsets)
      ;; FIX: search the rest of the keys/values
      ))

(defun bdef-keys-pointing-to (bdef &optional (dictionary *bdef-dictionary*))
  "Get a list of all the keys in `*bdef-dictionary*' that point to this bdef."
  (alexandria:when-let ((bdef (ensure-bdef bdef)))
    (loop :for key :being :the hash-keys :of dictionary
       :using (hash-value value)
       :if (eq value bdef)
       :collect key)))

(defun bdef-free (bdef &optional (dictionary *bdef-dictionary*))
  "Free a buffer from the bdef dictionary, removing all keys that point to it."
  (etypecase bdef
    (symbol (bdef-free (bdef bdef)))
    (string (bdef-free (bdef bdef)))
    (bdef
     (cl-collider:buffer-free (bdef-buffer bdef))
     (let ((keys (bdef-keys-pointing-to bdef dictionary)))
       (dolist (key keys)
         (remhash key dictionary))))))

(defun bdef-set (key value &optional (dictionary *bdef-dictionary*))
  "Set the KEY in the bdef dictionary DICTIONARY to VALUE."
  (setf (gethash key dictionary) (bdef-key-cleanse value))
  (bdef-get key dictionary))

(defun bdef-load (object &key (num-channels 2) (wavetable nil) (start-frame 0))
  "Load an object into a bdef, inserting the relevant keys into the bdef dictionary."
  (etypecase object
    (sc::env ;; FIX: we shouldn't assume that the user always wants an env to be a wavetable...
     (let* ((wavetable (or wavetable 512)) ;; FIX: if WAVETABLE is t...
            (buffer (buffer-alloc (* 2 wavetable) :chanls num-channels))
            (bdef (make-instance 'bdef
                                 :key object
                                 :buffer buffer
                                 :metadata (list
                                            :env object))))
       (buffer-setn buffer (list-in-wavetable-format (env-as-signal object wavetable)))
       bdef))
    (string
     (let* ((file (bdef-key-cleanse object))
            (buffer (buffer-read-any file :num-channels num-channels :wavetable wavetable :start-frame start-frame))
            (bdef (make-instance 'bdef
                                 :key file
                                 :buffer buffer
                                 :metadata (list))))
       (bdef-set file bdef)
       (loop :for (key function) :on *bdef-auto-metadata-list* :by #'cddr
          :do (bt:make-thread
               (lambda ()
                 (setf (bdef-metadata bdef key) (funcall function bdef)))))
       bdef))
    (list ;; FIX: this needs to work properly when :wavetable is t
     (let* ((buffer (buffer-alloc (length object)))
            (bdef (make-instance 'bdef
                                 :key object
                                 :buffer buffer
                                 :metadata (list))))
       (buffer-setn buffer object)
       bdef))))

;;; auto-metadata

(defvar *bdef-auto-metadata-list* nil
  "A plist of keys that will automatically be populated in a bdef's metadata for all newly-created or loaded buffers. The value for each key is the function that generates the value of the key for the bdef metadata.")

(defun define-bdef-auto-metadata (key function) ;; FIX: make into a macro instead?
  (setf (getf *bdef-auto-metadata-list* key) function))

(define-bdef-auto-metadata :onsets
    (lambda (bdef)
      (unless (< (frames bdef) 2000)
        (aubio-onsets bdef))))

(define-bdef-auto-metadata :tempo
    (lambda (bdef)
      (let* ((path (path bdef))
             (bpm (or
                   (extract-bpm-from-string path)
                   (extract-bpm-from-file-metadata path)
                   (bpm-tools-bpm path))))
        (when bpm
          (/ bpm 60)))))

;; (define-bdef-auto-metadata :dur (lambda (bdef)
;;                              (loop :until (bdef-metadata bdef :tempo)
;;                                 :do (sleep 0.1))
;;                              ;; FIX
;;                              ))

;; FIX: add 'metadata' key. it should probably add to the default metadata fields. if the user doesn't want them, they can provide 'nil' for the key they don't want generated.
(defun bdef (key &optional (value nil value-provided-p) &key (num-channels 2) (wavetable nil) (start-frame 0) metadata) ;; FIX: start-frame key doesn't work yet
  "Automaticaly load a buffer or reference one that's already loaded. KEY is the name to give the buffer in the bdef dictionary. VALUE is the path to the file to load, or the data to construct the buffer from (i.e. an envelope, a list of frames, etc).

Without a VALUE, bdef will look up the key and return the buffer that already exists. If the KEY is a string, it's assumed to be a pathname and will be loaded automatically if it's not already in memory."
  (assert (not (and value
                    (stringp key)))
          (key)
          "Cannot use a string as a key.")
  (when (typep key 'bdef)
    (return-from bdef key))
  ;; (when (/= 0 start-frame) ;; needs to be fixed in `buffer-read-any'
  ;;   (warn "bdef cannot yet load sounds with a start-frame."))
  (let ((key (bdef-key-cleanse key))
        (value (bdef-key-cleanse value)))
    (if value-provided-p
        (let ((res (bdef-set key (if (bdef-get value)
                                     value
                                     (bdef-load value :num-channels num-channels :wavetable wavetable :start-frame start-frame)))))
          (loop :for (key value) :on metadata :by #'cddr
             :do (setf (bdef-metadata res key) value))
          res)
        (or (bdef-get key)
            (if (symbolp key)
                (error "No bdef with the key ~a defined." key)
                (bdef-load key))))))

(defun bdef.new (&rest args) ;; (value nil value-provided-p) &key (num-channels 2) (wavetable nil) (start-frame 0) metadata
  "Automaticaly load a buffer or reference one that's already loaded. KEY is the name to give the buffer in the bdef dictionary. VALUE is the path to the file to load, or the data to construct the buffer from (i.e. an envelope, a list of frames, etc).

Without a VALUE, bdef will look up the key and return the buffer that already exists. If the KEY is a string, it's assumed to be a pathname and will be loaded automatically if it's not already in memory."
  (let* ((key (car args))
         (key-is-value (or (stringp key)
                           (pathnamep key)
                           (typep key 'cl-collider:env)))
         (value (if key-is-value
                    key
                    (elt args 1)))
         (options (if key-is-value
                      (cdr args)
                      (cddr args))
           ))
    (if (= 1 (length args))
        (format t "key lookup: ~s~%" key)
        (if (oddp (length options))
            (error "Invalid arguments for bdef.new: ~s" args)
            (format t "key: ~s; value: ~s; options: ~s~%" key value options)))))

;; (progn
;;   (fresh-line)
;;   (bdef.new :foo)
;;   (bdef.new :foo "/path")
;;   (bdef.new :foo "/path" :arg1 4 :arg2 99)
;;   (bdef.new "/path" :arg1 4 :arg2 99)
;;   (bdef.new "/path")
;;   (bdef.new (env (a -1 1 -1) (a 1 1)) :wavetable 1024 :arg2 99)
;;   (bdef.new :name (env (a -1 1 -1) (a 1 1)) :wavetable 1024 :arg2 99)
;;   )

(defmethod buffer-dur ((bdef bdef))
  (buffer-dur (bdef-buffer bdef)))

