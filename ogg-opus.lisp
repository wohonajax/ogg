;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:ogg)

(define-binary-type q7.8 ()
  (:reader (in)
    (* (read-value 'u2 in) (expt 2 -8)))
  (:writer (out value)
    (write-value 'u2 out (* value (expt 2 8)))))

(define-tagged-binary-class opus ()
  ((magick (ascii-string :length 8)))
  (:dispatch
   (alexandria:eswitch (magick :test #'string=)
     ("OpusHead" 'opus-id-header)
     ("OpusTags" 'opus-comment-header))))

(define-binary-class opus-id-header (opus)
  ((version u1)
   (audio-channels u1)
   (pre-skip u2)
   (input-sample-rate u4)
   (output-gain q7.8)
   (channel-mapping u1)))

(define-binary-class opus-comment-header (opus)
  ((vendor-string-length u4)
   (vendor-string (utf8-string :length vendor-string-length))
   (comment-list-length u4)
   (comments (comments :length comment-list-length))))
