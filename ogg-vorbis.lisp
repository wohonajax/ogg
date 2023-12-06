;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:ogg)



(defun unpack-float32 (x)
  (let ((mantissa (logand x #x1fffff))
        (sign (logand x #x80000000))
        (exponent (ash (logand x #x7fe00000) -21)))
    (unless (zerop sign)
      (setf mantissa (- mantissa)))
    (* mantissa (expt 2 (- exponent 788)))))

(defun low-neighbor (vector x)
  (loop for n across vector
        when (and (< n x)
                  (< (aref vector n)
                     (aref vector x)))
          maximize n))

(defun high-neighbor (vector x)
  (loop for n across vector
        when (and (< n x)
                  (> (aref vector n)
                     (aref vector x)))
          minimize n))

(defun render-point (x0 y0 x1 y1 x)
  (let* ((y-delta (- y1 y0))
         (x-delta (- x1 x0))
         (err (* (abs y-delta) (- x x0)))
         (off (truncate err x-delta)))
    (if (minusp y-delta)
        (- off y0)
        (+ off y0))))

(defun render-line (x0 y0 x1 y1 vector)
  (let* ((y-delta (- y1 y0))
         (x-delta (- x1 x0))
         (base (truncate y-delta x-delta))
         (x x0)
         (y y0)
         (err 0)
         (sy (if (minusp y-delta)
                 (1- base)
                 (1+ base))))
    (setf (aref vector x) y)
    (loop for x from (1+ x0) to (1- x1)
          do (incf err (abs y-delta))
          (if (>= err x-delta)
              (progn (decf err x-delta)
                     (incf y sy))
              (incf y base))
          (setf (aref vector x) y)))
  vector)

(define-binary-type utf8-string (length)
  (:reader (in)
           (babel:octets-to-string
            (read-value 'vector in :length length)
            :encoding :utf-8))
  (:writer (out value)))

(define-tagged-binary-class vorbis ()
  ((packet-type u1)
   (magick (ascii-string :length 6)))
  (:dispatch
   (ecase packet-type
     (1 'vorbis-id-header)
     (3 'vorbis-comment-header)
     (5 'vorbis-setup-header))))

(define-binary-class vorbis-id-header (vorbis)
  ((version u4)
   (audio-channels u1)
   (audio-sample-rate u4)
   (bitrate-maximum u4)
   (bitrate-nominal u4)
   (bitrate-minimum u4)
   (block-size u1)
   (framing-flag u1)))

(define-binary-class comment ()
  ((comment-length u4)
   (comment (utf8-string :length comment-length))))

(defun parse-comment-string (string)
  (let ((=-position (position #\= string)))
    (assert (numberp =-position))
    (list (intern (nstring-upcase (subseq string 0 =-position)) :keyword)
          (subseq string (1+ =-position)))))

(defun format-comments (comments)
  (loop for (key value) on comments by #'cddr
        collect (format nil "~:@(~a~)=~a" key value)))

(define-binary-type comments (length)
  (:reader (in)
           (loop repeat length
                 nconc (parse-comment-string
                        (comment (read-value 'comment in)))))
  (:writer (out value)))

(define-binary-type n-things (length thing)
  (:reader (in)
           (loop repeat length
                 collect (read-value thing in)))
  (:writer (out value)))

(define-binary-class vorbis-comment-header (vorbis)
  ((vendor-length u4)
   (vendor-string (utf8-string :length vendor-length))
   (comments-length u4)
   (comments (comments :length comments-length))
   (framing-flag u1)))

(define-binary-class vorbis-setup-header (vorbis)
  ((vorbis-codebook-count u1)
   (codebooks (n-things :thing 'codebook
                        :length (incf vorbis-codebook-count)))
   (vorbis-time-count (n-bits :n 6))
   ;; TODO: assert every zerop
   (vorbis-time-values (n-things :length (incf vorbis-time-count) :thing 'u2))
   (vorbis-floor-count (n-bits :n 6))
   (vorbis-floor-configurations (floor-configuration :length
                                                     (incf vorbis-floor-count)))
   ))

(define-binary-class codebook ()
  ((sync-pattern u3)
   (dimensions u2)
   (codebook-length u3)
   (ordered 1-bit)
   (codebook-entries (codebook-entries :length codebook-length
                                       :ordered ordered))
   (lookup-type (n-bits :n 4))
   (lookup (lookup :type lookup-type
                   :length codebook-length
                   :dimensions dimensions))))

(defun read-unordered-codebook-entries (length stream)
  (let ((result (make-array length)))
    (loop for i below length
          with sparse = (read-bit stream)
          do (setf (aref result i)
                   (if (and sparse
                            (not (read-bit stream)))
                       :unused
                       (1+ (read-n-bits 5 stream)))))
    result))

(defun ilog (x)
  (let ((value 0))
    (when (plusp x)
      (setf value (integer-length x)))
    value))

(defun read-ordered-codebook-entries (length stream)
  (let ((result (make-array length)))
    (loop for current-length from (1+ (read-n-bits 5 stream))
          for current-entry = 0 then (+ current-entry number)
          for number = (cond ((> current-entry length)
                              (error "Can't happen"))
                             ((= current-entry length)
                              (return))
                             (t
                              (read-n-bits (ilog (- length current-entry))
                                           stream)))
          do (loop for i from current-entry below (+ current-entry number)
                   do (setf (aref result i) current-length)))
    result))

(define-binary-type codebook-entries (length ordered)
  (:reader (in)
           (if ordered
               (read-ordered-codebook-entries length in)
               (read-unordered-codebook-entries length in)))
  (:writer (out value)))

(defun lookup1-values (length dimensions)
  (let ((r (floor (exp (/ (log length) dimensions)))))
    (if (>= length (expt (1+ r) dimensions))
        (1+ r)
        r)))

(defun read-lookup-values (type length dimensions stream)
  (let* ((min (unpack-float32 (read-value 'u4 stream)))
         (delta (unpack-float32 (read-value 'u4 stream)))
         (bits (1+ (read-n-bits 4 stream)))
         (sequencep (read-bit stream))
         (size (if (= type 1) ; size = codebook_lookup_values
                   (lookup1-values length dimensions)
                   (* length dimensions)))
         ;; TODO: lookup-offset
         ;; (we do have length (codebook-length)...)
         ;; lookup-offset = the current entry number in this codebook
         ;; one problem is that codebook-entries are a single vector
         ;; and aren't split into separate entries
         ;; see section 3.2 of the spec
         (lookup-offset)
         (result (make-array size)))
    (loop for i below size
          do (setf (aref result i)
                   (read-n-bits bits stream)))
    (flet ((decode-vq-1 (vector)
             (let ((last 0)
                   (value-vector (make-array dimensions)))
               (loop for i below dimensions
                     for index-divisor = 1 then (* index-divisor size)
                     for offset = (mod (truncate lookup-offset index-divisor)
                                       size)
                     do (setf (aref value-vector i)
                              (* (aref vector offset) (+ min delta last)))
                     (when sequencep
                       (setf last (aref value-vector i))))
               value-vector))
           (decode-vq-2 (vector)
             (let ((last 0)
                   (value-vector (make-array dimensions)))
               (loop for i below dimensions
                     for offset from (* lookup-offset dimensions)
                     do (setf (aref value-vector i)
                              (* (aref vector offset) (+ min delta last)))
                     (when sequencep
                       (setf last (aref value-vector i))))
               value-vector)))
      result)))

(define-binary-type lookup (type length dimensions)
  (:reader (in)
           (ecase type
             (0)
             ((1 2)
              (read-lookup-values type length dimensions in))))
  (:writer (out value)))

(define-binary-class floor-zero ()
  ((floor-zero-order u1)
   (floor-zero-rate u2)
   (floor-zero-bark-mapsize u2)
   (floor-zero-amplitude-bits (n-bits :n 6))
   (floor-zero-amplitude-offset u1)
   (floor-zero-number-of-books (n-bits :n 4))
   (floor-zero-book-list (n-things :length floor-zero-number-of-books
                                   :thing 'u1))))

(defun decode-floor-zero-packet (floor-zero stream)
  (let ((amplitude (read-n-bits (floor-zero-amplitude-bits floor-zero) stream)))
    (if (plusp amplitude)
        (let ((coefficients (make-array 0))
              (booknumber (read-n-bits
                           (ilog (floor-zero-number-of-books floor-zero))
                           stream))
              (last 0)
              (book-list (floor-zero-book-list floor-zero)))
          (if (> booknumber (reduce #'max book-list))
              (error "Undecodable")
              (loop for coefficients-length = (length coefficients)
                      then (length coefficients)
                    for last = 0
                      then (if (zerop coefficients-length)
                               0
                               (aref coefficients (1- coefficients-length)))
                    while (< coefficients-length
                             (floor-zero-order floor-zero))
                    do (loop for i below booknumber
                             with temp-vector = (make-array)
                             do (setf (aref temp-vector i)
                                      ;; FIXME: see section 6.2.2 of the spec
                                      ;; we need to read a vector from STREAM
                                      ;; "using codebook number [floor0_book_list]
                                      ;; element [booknumber] in VQ context"
                                      (read-n-bits (nth booknumber book-list)
                                                   stream)))
                       (setf coefficients
                             (concatenate 'vector coefficients temp-vector))))
          :unused))))

(defun product (init n fn &rest args)
  (loop for j from init upto n
        collect (apply fn j args) into result
        finally (reduce #'* result)))

(defun compute-floor-zero-curve (floor-zero amplitude coefficients n)
  (let* ((map-size (floor-zero-bark-mapsize floor-zero))
         (rate (floor-zero-rate floor-zero))
         (order (floor-zero-order floor-zero))
         (amp-offset (floor-zero-amplitude-offset floor-zero))
         (amp-bits (floor-zero-amplitude-bits floor-zero)))
    (flet ((bark (x)
             (+ (* 13.1 (atan (* .00074 x)))
                (* 2.24 (atan (* .0000000185 x x)))
                (* .0001 x)))
           (p-fn (j omega)
             (* 4 (expt (- (cos (aref coefficients (1+ (* 2 j))))
                           (cos omega))
                        2)))
           (q-fn (j omega)
             (* 4 (expt (- (cos (aref coefficients (* 2 j)))
                           (cos omega))
                        2))))
      (let ((map (loop for i from 0 upto n
                       with foobar = (* (bark (/ (* i rate)
                                                 (* 2 n)))
                                        (/ map-size
                                           (bark (* .5 rate))))
                       collect (if (= i n)
                                   -1
                                   (min (1- map-size) foobar)))))
        (let* ((i 0)
               (omega 0)
               (p 0)
               (q 0)
               linear-floor-value
               iteration-condition
               (result (make-array n)))
          (tagbody step2 (setf omega (* pi (nth i map)))
             (if (oddp order)
                 (setf p (* (- 1 (expt (cos omega) 2))
                            (product (/ (- order 3) 2)
                                     #'p-fn omega))
                       q (* (/ 4) (product (/ (1- order) 2)
                                           #'q-fn omega)))
                 (setf p (* (/ (- 1 (cos omega)) 2)
                            (product (/ (- order 2) 2)
                                     #'p-fn omega))
                       q (* (/ (1+ (cos omega)) 2)
                            (product (/ (- order 2) 2)
                                     #'q-fn omega))))
             (setf linear-floor-value
                   (exp (* .11512925
                           (- (/ (* amplitude amp-offset)
                                 (* (1- (expt 2 amp-bits))
                                    (sqrt (+ p q))))
                              amp-offset))))
             step5 (setf iteration-condition (nth i map))
             (setf (aref result i) linear-floor-value)
             (incf i)
             (when (= (nth i map) iteration-condition)
               (go step5))
             (when (< i n)
               (go step2))
             result))))))

(defun read-floor-one (stream)
  ;; header decode
  ;; see section 7.2.2 of the spec
  (loop with partitions = (read-n-bits 5 stream)
        with partition-class-list = (make-array partitions)
        for maximum-class = -1
          then (loop for x across partition-class-list
                     when (integerp x)
                       maximize x)
        for i below partitions
        do (setf (aref partition-class-list i) (read-n-bits 4 stream))
           (loop with class-dimensions = (make-array maximum-class)
                 with subclasses = (make-array maximum-class)
                 with masterbooks = (make-array maximum-class)
                 for i below maximum-class
                 do (setf (aref class-dimensions i) (1+ (read-n-bits 3 stream))
                          (aref subclasses i) (read-n-bits 2 stream))
                 when (plusp (aref subclasses i))
                   do (setf (aref masterbooks i) (read-n-bits 8 stream))
                      (loop with max = (1- (expt 2 (aref subclasses i)))
                            with subclass-books
                              = (make-array (list maximum-class max))
                            for j below max
                            do (setf (aref subclass-books i j)
                                     (1- (read-n-bits 8 stream)))
                               (loop with multiplier = (1+ (read-n-bits 2 stream))
                                     with range = (read-n-bits 4 stream)
                                     with x-list = (make-array 2 :adjustable t) ; TODO: dimensions?
                                     for values from 2
                                     initially (setf (aref x-list 0) 0
                                                     (aref x-list 1) (expt 2 range))
                                     for a below partitions
                                     for current-class-number
                                       = (aref partition-class-list a)
                                     do (loop for b below
                                                    (aref class-dimensions
                                                          current-class-number)
                                              do (setf (aref x-list values)
                                                       (read-n-bits range stream))))))))

(define-binary-type floor-configuration (length)
  (:reader (in)
           (let ((result (make-array length)))
             (loop for i below length
                   for floor-type = (read-value 'u2 in)
                     then (read-value 'u2 in)
                   do (setf (aref result i)
                            (cond ((zerop floor-type)
                                   (decode-floor-zero-packet))
                                  ((= floor-type 1)
                                   (decode-floor-one-packet))
                                  ((> floor-type 1)
                                   (error "Can't happen"))))
                   finally (return result))))
  (:writer (out value)))
