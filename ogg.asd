;;; -*- Mode: Lisp -*-

(asdf:defsystem #:ogg
  :serial t
  :depends-on (com.gigamonkeys.binary-data
               trivial-gray-streams
               babel
               alexandria)
  :components ((:file "packages")
               (:file "ogg-page")
               (:file "ogg-vorbis")
               (:file "ogg-opus")))
