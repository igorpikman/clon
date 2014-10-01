;;;; -*- Mode: Lisp -*-

(cl:defpackage #:clon-test-system
  (:use #:asdf #:cl))

(cl:in-package #:clon-test-system)

(defsystem :clon-test
  :depends-on (#:clon)
  :components ((:file "test-packages")
               (:file "clon-test"))
  :serial t)
