;;;; bdef.lisp
;; FIX: allow a sound to be loaded as a wavetable and a normal sound at the same time
;; FIX: also ensure a sound can be loaded multiple times with different number of channels
;; FIX: make all methods like `splits-bdef', `bdef-buffer', `bdef-sample-rate', etc take an errorp argument to determine whether to error if the data is not available
;; FIX: auto-metadata should just become an interface over a more general hook mechanism

(in-package #:bdef)

;;; global variables/configuration

(defvar *bdef-temporary-directory*
  (merge-pathnames "bdef/" (uiop:temporary-directory))
  "The directory bdef should store its temporary files in (i.e. the .wav files generated from format auto-conversion).")

(defvar *ffmpeg-path*
  #+(or linux darwin) (ignore-errors (uiop:run-program "which ffmpeg" :output (list :string :stripped t)))
  #+windows (concat (uiop:getenv-pathname "USERPROFILE") "/AppData/Local/")
  "The path to ffmpeg, or nil if ffmpeg could not be found.")

(defvar *ffprobe-path*
  #+(or linux darwin) (ignore-errors (uiop:run-program "which ffprobe" :output (list :string :stripped t)))
  #+windows (concat (uiop:getenv-pathname "USERPROFILE") "/AppData/Local/")
  "The path to ffprobe, or nil if ffprobe could not be found.")

(defvar *bdef-backends* (list)
  "The list of enabled backends. When the user attempts to create a bdef, each backend in this list is used to try to create the bdef from the file or object. If a backend returns nil, the next backend in the list will be tried. This is repeated until the first backend successfully returns a bdef object.

Note that backends are made available by loading the relevant bdef subsystem with Quicklisp or ASDF. For example, if you want to use SuperCollider/cl-collider, load `bdef/cl-collider'. Once you've loaded it, you should see that the :cl-collider keyword is in this list.")

;;; file handling

(define-condition file-does-not-exist (simple-condition file-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<File ~S does not exist.~@:>" (file-error-pathname condition)))))

(defun ensure-namestring (filename)
  "Get the full path to FILENAME."
  ;; uiop:native-namestring is used twice to ensure path abbreviations (like ~ for $HOME) are expanded.
  (uiop:native-namestring (uiop:native-namestring (uiop:ensure-pathname filename :want-non-wild t))))

(defun file-exists-p (filename)
  "True if FILENAME names a file that exists. This function is needed to ensure characters like ? are not interpreted as Common Lisp pathname wildcards."
  (probe-file (uiop:ensure-pathname filename :want-non-wild t)))

(defun ensure-readable-audio-file (file &key num-channels (backend (first *bdef-backends*)) (extensions (bdef-backend-supported-file-types backend)))
  "Use ffmpeg to convert FILE to the first format in EXTENSIONS if it is not already in one of those formats. Returns the path to the converted file (or FILE if no conversion is needed), and the file's metadata. The converted file is stored under `*bdef-temporary-directory*'.

See also: `file-metadata', `*ffmpeg-path*', `*bdef-temporary-directory*'"
  (let* ((file (ensure-namestring file))
         (filename (nth-value 2 (uiop:split-unix-namestring-directory-components file)))
         (ext (pathname-type file))
         (file-metadata (file-metadata file))
         (num-channels (or num-channels (getf file-metadata :channels)))
         (extensions (ensure-list extensions)))
    (values
     (if (and (position ext extensions :test #'string-equal)
              (eql num-channels (getf file-metadata :channels)))
         (if (file-exists-p file)
             file
             (error 'file-does-not-exist :pathname file))
         (let* ((output-directory (merge-pathnames (concat num-channels "-channel/") *bdef-temporary-directory*))
                (output-filename (concat output-directory
                                         (uiop:split-name-type filename)
                                         "." (string-downcase (car extensions)))))
           (unless (file-exists-p output-filename)
             (if *ffmpeg-path*
                 (unless (file-exists-p *ffmpeg-path*)
                   (error "ffmpeg binary not found at ~s; you will need to update ~s to point to ffmpeg." *ffmpeg-path* '*ffmpeg-path*))
                 (error "~s is nil; you will need to set it to point to your ffmpeg binary." '*ffmpeg-path*))
             (unless (file-exists-p file)
               (error 'file-does-not-exist :pathname file))
             (ensure-directories-exist output-directory)
             (uiop:run-program (list *ffmpeg-path*
                                     "-i" file
                                     "-ac" (write-to-string num-channels)
                                     output-filename)))
           output-filename))
     file-metadata)))

(defun file-metadata (file)
  "Get the metadata of FILE as a plist."
  (let* ((json (jsown:parse
                (uiop:run-program (list *ffprobe-path* "-v" "quiet" "-print_format" "json" "-show_format" "-show_streams" (ensure-namestring file))
                                  :output (list :string :stripped t)
                                  :ignore-error-status t)))
         (streams (jsown:filter json "streams"))
         (audio-stream (find-if (fn (string-equal "audio" (jsown:filter _ "codec_type"))) streams))
         (res (list :channels (jsown:filter audio-stream "channels")))
         (format (jsown:val json "format")))
    (when (jsown:keyp format "tags")
      (jsown:do-json-keys (k v) (jsown:val format "tags")
        (push v res)
        (push (friendly-symbol k) res)))
    res))

;;; backend generics

(defgeneric bdef-backend-supported-file-types (backend)
  (:documentation "Get a list of file extensions as keywords naming the file types that the backend natively supports (i.e. that don't need conversion).

See also: `bdef-backend-load', `ensure-readable-audio-file'"))

(defgeneric bdef-backend-load (backend object &key)
  (:documentation "Load a file or other object into a bdef via the specified backend. This method should return the backend's buffer object. OBJECT will either be a string (the path to the file to load) or a list of strings (if the files should be loaded as consecutive buffers, i.e. for SuperCollider's VOsc UGen). Requested files will always be in a format that the backend reports supporting via the `bdef-backend-supported-file-types' method.

See also: `bdef-backend-free', `bdef-backend-supported-file-types'"))

(defmethod bdef-backend-load (backend (object pathname) &rest args &key &allow-other-keys)
  (apply #'bdef-backend-load backend (namestring object) args))

(defgeneric bdef-backend-free (buffer)
  (:documentation "Free a buffer via its backend.

See also: `bdef-backend-load'"))

;;; auto-metadata

(defvar *bdef-auto-metadata* (list)
  "Plist of keys that will automatically be populated in a bdef's metadata for all newly-created or loaded buffers. The value for each key is the function that generates the value of the key for the bdef metadata. Use the `define-bdef-auto-metadata' macro or `set-bdef-auto-metadata' function to define bdef-auto-metadata keys, or `remove-bdef-auto-metadata' to remove them.")

(defun set-bdef-auto-metadata (key function)
  "Add KEY as an auto-metadata key for bdefs. FUNCTION will be run with the bdef as its argument, and the result will be set to the bdef's metadata for KEY."
  (setf (getf *bdef-auto-metadata* key) function))

(defmacro define-bdef-auto-metadata (key &body body)
  "Define an auto-metadata key for bdefs. The variable BDEF will be bound in BODY to the bdef in question."
  `(setf (getf *bdef-auto-metadata* ,key)
         (lambda (bdef) ,@body)))

(defun remove-bdef-auto-metadata (key)
  "Remove a previously-defined auto-metadata key."
  (remove-from-plistf *bdef-auto-metadata* key))

(define-bdef-auto-metadata :onsets
  (unless (< (bdef-duration bdef) 1)
    (splits-from-aubio bdef :utility :onset)))

(define-bdef-auto-metadata :tempo
  (when-let* ((path (bdef-file bdef))
              (bpm (or (string-extract-bpm path)
                       (bpm-tools-bpm path))))
    (/ bpm 60)))

(define-bdef-auto-metadata :dur
  (when-let ((tempo (bdef-metadata bdef :tempo)))
    (round (* tempo (bdef-duration bdef)))))

(define-bdef-auto-metadata :splits
  (when-let ((original-file (bdef-metadata bdef :original-file)))
    (when (member (pathname-type (pathname original-file)) (list "aif" "aiff") :test #'string-equal)
      (splits-from-op-1-drumset original-file :if-invalid nil))))

;;; dynamic metadata

(defvar *bdef-dynamic-metadata* nil
  "Alist of dynamic metadata keys and their get and set functions.")

(defmacro define-bdef-dynamic-metadata (key get-body set-body)
  "Define a metadata key that is calculated dynamically. For example, bpm is just tempo multiplied by 60.

KEY is the name of the key, GET-BODY is the function to run to get the key, and SET-BODY is the function to run to set metadata when (setf (bdef-metadata bdef KEY) ...) is called.

See also: `define-bdef-auto-metadata', `bdef-metadata'"
  `(push (list (make-keyword ',key)
               (lambda (bdef) ,get-body)
               (lambda (bdef value) ,set-body))
         *bdef-dynamic-metadata*))

(define-bdef-dynamic-metadata bpm
    (when-let ((tempo (bdef-metadata bdef :tempo)))
      (* 60 tempo))
  (setf (bdef-metadata bdef :tempo) (/ value 60)))

(defun remove-bdef-dynamic-metadata (key)
  "Remove a key from the bdef dynamic metadata list."
  (let ((key (make-keyword key)))
    (setf *bdef-dynamic-metadata* (remove-if
                                   (lambda (item) (eql (car item) key))
                                   *bdef-dynamic-metadata*))))

;;; bdef

(defgeneric bdef-name (bdef)
  (:documentation "The name (key) of the bdef."))

(defgeneric bdef-id (bdef)
  (:documentation "The ID number of this bdef, or nil if the bdef's backend does not support IDs."))

(defgeneric bdef-buffer (object)
  (:documentation "The actual buffer object that the bdef refers to."))

(defgeneric bdef-metadata (bdef &optional key)
  (:documentation "Get the value of BDEF's metadata for KEY, or the full metadata hashtable if KEY is not provided. Returns true as a second value if the metadata had an entry for KEY, or false if it did not.

Note that this function will block if the specified metadata is one of the `*bdef-auto-metadata*' that hasn't finished being generated yet."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass bdef ()
    ((name :initarg :name :reader bdef-name :type string-designator :documentation "The name (key) of the bdef.")
     (buffer :initarg :buffer :reader bdef-buffer :documentation "The actual buffer object that the bdef refers to.")
     (metadata :initarg :metadata :initform (make-hash-table) :type hash-table :documentation "Hash table of additional data associated with the bdef, accessible with the `bdef-metadata' function."))))

(define-dictionary bdef :name-type string-designator)

(defmethod print-object ((bdef bdef) stream)
  (with-slots (name buffer) bdef
    (if (slot-boundp bdef 'name)
        (format stream "(~s ~s)" 'bdef name)
        (format stream "#<~s>" 'bdef))))

(defmethod describe-object ((bdef bdef) stream)
  (with-slots (name buffer metadata) bdef
    (format stream "~s is a ~s.~%  It is ~s seconds long (~s frames), with ~s channels.~%" bdef 'bdef (bdef-duration bdef) (bdef-length bdef) (bdef-channels bdef))
    (format stream "~@[  It contains the audio from the file ~s.~%~]" (bdef-file bdef))
    (format stream "  Names that point to this buffer are: ~s~%" (bdef-names bdef))
    (when-let ((meta-keys (keys (bdef-metadata bdef))))
      (format stream "  It has the following metadata:~%")
      (dolist (key meta-keys)
        (format stream "    ~s -> ~s~%" key (bdef-metadata bdef key))))))

;; FIX: add 'metadata' key. it should probably add to the default metadata fields. if the user doesn't want them, they can provide 'nil' for the key they don't want generated.
(defun bdef (name &optional (value nil value-provided-p) &key (num-channels 2) wavetable metadata backend (dictionary *bdef-dictionary*))
  "Automatically load a buffer or reference one that's already loaded. NAME is the name to give the buffer in the bdef dictionary. VALUE is the path to the file to load, or the data to construct the buffer from (i.e. an envelope, a list of frames, etc).

Without VALUE, bdef will look up and return the bdef that already exists with the given name. If NAME is a pathname or string, it's assumed to be the name of a file, which is automatically loaded if necessary."
  (check-type name (or bdef pathname (and string-designator (not null))))
  (assert (not (equal name value)) (name value) "Cannot set a bdef to itself.")
  (let ((name (if (pathname-designator-p name)
                  (ensure-namestring name)
                  name)))
    (unless value-provided-p ; FIX: read the file if only a filename is provided and it hasn't been read yet
      (return-from bdef (if-let ((bdef (find-bdef name :errorp nil :dictionary dictionary)))
                          bdef
                          (if (stringp name)
                              (bdef name name)
                              (find-bdef name :dictionary dictionary)))))
    (setf (find-bdef name) (etypecase value
                             (symbol (ensure-namestring value))
                             (bdef value)
                             ((or string pathname)
                              (let* ((path (ensure-namestring value))
                                     (bdef (bdef-load path :num-channels num-channels :wavetable wavetable :backend backend)))
                                (if (stringp name)
                                    bdef
                                    path)))))
    (when-let ((bdef (find-bdef name :dictionary dictionary)))
      (doplist (key value metadata bdef)
        (setf (bdef-metadata bdef key) (if (functionp value)
                                           (funcall value bdef)
                                           value))))))

(uiop:with-deprecation (:error)
  (defun bdef-ref (name &optional (dictionary *bdef-dictionary*))
    "Deprecated alias for `find-bdef'."
    (find-bdef name :dictionary dictionary))

  (defun (setf bdef-ref) (value name &optional (dictionary *bdef-dictionary*))
    "Deprecated alias for `(setf find-bdef)'."
    (setf (find-bdef name :dictionary dictionary) value))

  (defun bdef-get (key &optional (dictionary *bdef-dictionary*))
    "Deprecated alias for `find-bdef'."
    (find-bdef key :dictionary dictionary))

  (defun bdef-set (key value &optional (dictionary *bdef-dictionary*))
    "Deprecated alias for `(setf find-bdef)'."
    (setf (find-bdef key :dictionary dictionary) value))

  (defun bdef-key (object)
    "Deprecated alias for `bdef-name'."
    (bdef-name object))

  (defun (setf bdef-key) (value object)
    "Deprecated alias for `(setf bdef-name)'."
    (setf (bdef-name object) value)))


(defmethod bdef-id ((name symbol))
  (bdef-id (bdef-buffer (find-bdef name))))

(defmethod bdef-id ((name string))
  (bdef-id (bdef-buffer (find-bdef name))))

(defmethod bdef-id ((null null))
  nil)

(defmethod bdef-id ((bdef bdef))
  (bdef-id (bdef-buffer bdef)))

(defmethod bdef-metadata ((name symbol) &optional key)
  (bdef-metadata (find-bdef name) key))

(defmethod bdef-metadata ((name string) &optional key)
  (bdef-metadata (find-bdef name) key))

(defmethod bdef-metadata ((null null) &optional key)
  nil)

(defmethod bdef-metadata ((bdef bdef) &optional key)
  (if key
      (if-let ((dyn-meta (assoc key *bdef-dynamic-metadata*)))
        (funcall (cadr dyn-meta) bdef)
        (multiple-value-bind (val present-p) (gethash key (slot-value bdef 'metadata))
          (values
           (if val
               (if (typep val 'eager-future2:future)
                   (setf (bdef-metadata bdef key) (eager-future2:yield val))
                   val)
               val)
           present-p)))
      (slot-value bdef 'metadata)))

(defmethod (setf bdef-metadata) (value (symbol symbol) &optional key)
  (setf (bdef-metadata (find-bdef symbol) key) value))

(defmethod (setf bdef-metadata) (value (string string) &optional key)
  (setf (bdef-metadata (find-bdef string) key) value))

(defmethod (setf bdef-metadata) (value (null null) &optional key)
  nil)

(defmethod (setf bdef-metadata) (value (bdef bdef) &optional key)
  (if key
      (if-let ((dyn-meta (assoc key *bdef-dynamic-metadata*)))
        (funcall (caddr dyn-meta) bdef value)
        (with-slots (metadata) bdef
          (when (and (typep value 'splits)
                     (null (splits-bdef value)))
            (setf (splits-bdef value) bdef)
            (unless (gethash :splits metadata)
              (setf (gethash :splits metadata) value)))
          (setf (gethash key metadata) value)))
      (setf (slot-value bdef 'metadata) (etypecase value
                                          (list (plist-hash-table value))
                                          (hash-table value)))))

(uiop:with-deprecation (:error)
  (defun bdef-metadata-keys (bdef)
    "Deprecated alias for (keys (bdef-metadata bdef))."
    (keys (bdef-metadata bdef))))

(defun bdef-splits (bdef) ; FIX: this should return all of them as a list
  "Get any `splits' from BDEF's metadata, searching in preferred order (i.e. :splits key first, etc).

See also: `bdef-splits-events', `splits-events'"
  (let ((bdef (find-bdef bdef)))
    (dolist (key (list :splits :onsets :beats))
      (when-let ((val (bdef-metadata bdef key)))
        (when (splits-p val)
          (return-from bdef-splits val))))))

(defun bdef-splits-events (bdef &key (unit :percents) (keys (list :start :end :dur :beat)) remove-keys)
  "Get the `splits' from BDEF's metadata (as per `bdef-splits') as a list of events (as per `splits-events').

See also: `bdef-splits', `splits-events'"
  (splits-events (bdef-splits bdef) :unit unit :keys keys :remove-keys remove-keys))

(defun (setf bdef-splits) (splits bdef)
  (setf (bdef-metadata (find-bdef bdef) :splits) splits))

(defgeneric bdef-load (object &key)
  (:documentation "Load a file or other object into a bdef. Different backends will support different object types, and different key arguments. Here are the key arguments supported by one or more backends:

- id - Requested buffer ID number.
- num-channels - Number of channels to load (defaults to 2).
- wavetable - Whether to load the object in wavetable format. If T, the buffer length will be the next power of two. If a number is provided, load the object as a wavetable of that size.
- metadata - Plist of metadata to include in the bdef."))

(defmethod bdef-load ((file string) &rest args &key backend (num-channels 2) (wavetable nil) id metadata)
  (declare (ignorable id wavetable))
  (let ((original-file (ensure-namestring file))
        (backend (or backend
                     (car *bdef-backends*)
                     (error "No bdef backends are currently enabled. Enable one by loading its subsystem.")))
        (num-channels (or num-channels
                          (if wavetable 1 2))))
    (unless (file-exists-p original-file)
      (error 'file-does-not-exist :pathname original-file))
    (multiple-value-bind (file file-metadata)
        (ensure-readable-audio-file original-file :num-channels num-channels :backend backend)
      (let* ((buffer (apply #'bdef-backend-load backend file :num-channels num-channels args))
             (bdef (make-instance 'bdef
                                  :name file
                                  :buffer buffer)))
        (setf (bdef-metadata bdef :original-file) original-file
              (find-bdef file) original-file
              (find-bdef original-file) bdef)
        (doplist (key value file-metadata)
          (unless (eql key :channels) ; the channels key is inserted by the `file-metadata' function for the number of channels the source (pre-conversion) has
            (if (member key (list :bpm :tbpm))
                (let ((parsed (parse-float:parse-float value)))
                  (unless (= 0 parsed)
                    (setf (bdef-metadata bdef :bpm) parsed)))
                (setf (bdef-metadata bdef key) value))))
        (doplist (key function *bdef-auto-metadata*)
          (unless (or (bdef-metadata bdef key) ; we trust the file's tags; no need to detect the bpm if we already know it
                      (getf metadata key))
            (let ((k key)
                  (f function))
              (setf (bdef-metadata bdef key)
                    (eager-future2:pcall
                     (lambda ()
                       (let ((value (funcall f bdef)))
                         (setf (bdef-metadata bdef k) value)
                         value)))))))
        (doplist (key value metadata bdef)
          (setf (bdef-metadata bdef key) value))))))

(defmethod no-applicable-method ((method (eql #'bdef-load)) &rest args)
  (error "None of the enabled bdef backends support loading ~s." (car args)))

(defun bdef-free (bdef &optional (dictionary *bdef-dictionary*))
  "Free BDEF's buffer and remove all names in DICTIONARY that point to it."
  (etypecase bdef
    (symbol (bdef-free (bdef bdef)))
    (string (bdef-free (bdef bdef)))
    (bdef
     (bdef-backend-free (bdef-buffer bdef))
     (let ((names (bdef-names bdef :dictionary dictionary)))
       (dolist (name names)
         (remhash name dictionary))))))

;;; bdef methods:
;; (these should be implemented for each bdef backend)

(defgeneric bdef-length (bdef)
  (:documentation "BDEF's length in frames."))

(defmethod bdef-length (bdef)
  (bdef-length (bdef-buffer bdef)))

(defgeneric bdef-sample-rate (bdef)
  (:documentation "BDEF's sample rate in Hz."))

(defmethod bdef-sample-rate (bdef)
  (bdef-sample-rate (bdef-buffer bdef)))

(defgeneric bdef-channels (bdef)
  (:documentation "The number of BDEF's channels."))

(defmethod bdef-channels (bdef)
  (bdef-channels (bdef-buffer bdef)))

(defgeneric bdef-file (bdef)
  (:documentation "The file that BDEF was loaded from, or nil if it was not loaded from a file."))

(defmethod bdef-file (bdef)
  (bdef-file (bdef-buffer bdef)))

(defgeneric bdef-frames (bdef &key start end channels)
  (:documentation "Get an array of the frames of BDEF's buffer, from START below END. CHANNELS specifies which channel(s) to get; if an integer, returns a single-dimensional array of the specified channel; if a list, returns a multi-dimensional array of the specified channels. Defaults to all frames of all channels.

See also: `bdef-frame'"))

(defmethod bdef-frames ((bdef bdef) &key (start 0) (end (bdef-length bdef)) channels)
  (bdef-frames (bdef-buffer bdef) :start start :end end :channels channels))

(uiop:with-deprecation (:error)
  (defun bdef-subseq (bdef &optional (start 0) (end (bdef-length bdef)) channel)
    "Deprecated alias for `bdef-frames'."
    (bdef-frames bdef :start start :end end :channels channel)))

;;; derived methods:
;; these can be overridden in a backend if a faster/more performant way is
;; possible, however you normally don't need to implement them as their default
;; methods fall back on the "required" bdef methods above.

(defgeneric bdef-duration (bdef)
  (:documentation "Get the duration of the bdef in seconds."))

(defmethod bdef-duration ((bdef t))
  (/ (bdef-length bdef) (bdef-sample-rate bdef)))

(defgeneric bdef-tempo (bdef)
  (:documentation "Get the tempo of the bdef in beats per second. Defaults to 1 if no tempo metadatum exists."))

(defmethod bdef-tempo (bdef)
  (or (bdef-metadata bdef :tempo) 1))

(defgeneric bdef-dur (bdef)
  (:documentation "Get the dur of the bdef in beats."))

(defmethod bdef-dur (bdef)
  (let ((time (bdef-duration bdef))
        (tempo (bdef-tempo bdef)))
    (* time tempo)))

(defgeneric bdef-frame (bdef index &optional channel)
  (:documentation "Get the value of the bdef frame at INDEX. Without CHANNEL, return an array of the specified index in all channels. With CHANNEL, return just the value as a number.

See also: `bdef-frames'"))

(defmethod bdef-frame (bdef index &optional channels)
  (let ((res (bdef-frames bdef :start index :end (1+ index) :channels channels)))
    (if (integerp channels)
        (aref res 0)
        (let ((len (apply #'* (array-dimensions res))))
          (make-array (list len)
                      :initial-contents (loop :for i :from 0 :repeat len
                                              :collect (row-major-aref res i)))))))

(uiop:with-deprecation (:error)
  (defun bdef-elt (bdef index &optional channel)
    "Deprecated alias for `bdef-frame'."
    (bdef-frame bdef index channel)))
