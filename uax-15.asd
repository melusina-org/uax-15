;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-USER -*-

(defpackage :uax-15-system
  (:use :common-lisp :asdf))
(in-package :uax-15-system)

(defparameter *string-file* "strings-utf-8")

(defsystem "uax-15"
  :description "Common lisp implementation of Unicode normalization functions :nfc, :nfd, :nfkc and :nfkd (Uax-15)"
  :author "Takeru Ohta, Sabra Crolleton <sabra.crolleton@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on ("split-sequence" "cl-ppcre" "uiop")
  :components
  ((:module "src"
            :components ((:file "package")
                         (:file "utilities" :depends-on ("package"))
                         (:file "trivial-utf-16" :depends-on ("package"))
                         (:file "precomputed-tables" :depends-on ("package" "utilities"))
                         (:file "normalize-backend" :depends-on ("package" "utilities" "precomputed-tables" "trivial-utf-16"))
                         (:file "uax-15" :depends-on ("package" "utilities" "normalize-backend" "trivial-utf-16")))))
  :in-order-to ((test-op (test-op "uax-15/tests"))))

(defsystem "uax-15/tests"
  :depends-on ("uax-15" "parachute" "uiop" "cl-ppcre" "split-sequence")
  :components
  ((:module "src"
            :components ((:file "trivial-utf-16")))
   (:module "t"
            :depends-on ("src")
            :components ((:file "test-package")
                         (:file "tests"))))
  :perform (test-op (o c)
		    (uiop:symbol-call :parachute '#:test (uiop:find-symbol* 'suite :uax-15-tests)
				      :report (uiop:find-symbol* 'quiet :parachute))))

