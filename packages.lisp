;;; -*- Mode: Lisp -*-

(defpackage #:ogg
  (:use #:cl #:binary-data
        #:trivial-gray-streams)
  (:export #:with-ogg-stream
           #:comments
           #:vorbis
           #:opus))
