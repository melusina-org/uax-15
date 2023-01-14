;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: UAX-15-TESTS -*-

(defpackage :uax-15/testsuite
  (:use :common-lisp :uax-15)
  (:import-from #:org.melusina.confidence
   #:define-testcase
   #:assert-equal
   #:assert-nil
   #:assert-t)
  (:export #:run-all-tests))

(in-package :uax-15/testsuite)

(defun parse-hex-string-to-string (str)
  "Takes a string which may be one or more hex numbers e.g. '0044 0307', builds an array of characters, coerces to string and returns the string. Mostly used for testing."
  (let* ((split-str (split-sequence:split-sequence #\Space str :remove-empty-subseqs t))
         (arry (make-array (length split-str))))
    (loop for x in split-str counting x into y do
         (setf (aref arry (- y 1)) (parse-integer x :radix 16)))
    (uax-15:from-unicode-string (coerce arry 'uax-15:unicode-string))))

(defun hs-to-cs (str)
  "Syntactic sugar"
  (parse-hex-string-to-string str))

(defparameter *test-directory* (uiop:merge-pathnames*
                                (make-pathname :directory (list :relative "t") :name nil :type nil)
                                (asdf:system-source-directory (asdf:find-system 'uax-15 nil))))

(defun read-test-data (fname)
  (with-open-file (in (uiop:merge-pathnames* *test-directory* fname))
    (loop for line = (read-line in nil nil)
       while line
       collect (cl-ppcre:split ";" line))))

(defun first-failure (fname fmt)
  "Reports the data line for the first failure for debugging purposes where fname is the test data filename and fmt is e.g. :nfkc"
  (loop for x in (read-test-data fname) counting x into y do
       (when (not (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) fmt)))
         (format t "Failure Line ~a ~a fourth should equal normalized first" y x)
         (return-from first-failure))))

(defparameter *part0* (read-test-data "test-part0-15.txt"))
(defparameter *part1* (read-test-data "test-part1-15.txt"))
(defparameter *part2* (read-test-data "test-part2-15.txt"))
(defparameter *part3* (read-test-data "test-part3-15.txt"))

(define-testcase validate-nfkc (x)
  (assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc))
  (assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc))
  (assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc))
  (assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc)))

(define-testcase part0-nfkc ()
  (loop for x in *part0* do (validate-nfkc x)))

(define-testcase part1-nfkc ()
  (loop for x in *part1* do (validate-nfkc x)))

(define-testcase part2-nfkc ()
  (loop for x in *part2* do (validate-nfkc x)))

(define-testcase part3-nfkc ()
  (loop for x in *part3* do (validate-nfkc x)))

(define-testcase all-nfkc ()
  (part0-nfkc)
  (part1-nfkc)
  (part2-nfkc)
  (part3-nfkc))

(define-testcase validate-nfkd (x)
  (assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd))
  (assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd))
  (assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd))
  (assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd)))

(define-testcase part0-nfkd ()
  (loop for x in *part0* do (validate-nfkd x)))

(define-testcase part1-nfkd ()
  (loop for x in *part1* do (validate-nfkd x)))

(define-testcase part2-nfkd ()
  (loop for x in *part2* do (validate-nfkd x)))

(define-testcase part3-nfkd ()
  (loop for x in *part3* do (validate-nfkd x)))

(define-testcase all-nfkd ()
  (part0-nfkd)
  (part1-nfkd)
  (part2-nfkd)
  (part3-nfkd))

(define-testcase validate-nfc (x)
  (assert-equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc))
  (assert-equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc))
  (assert-equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc))
  (assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc))
  (assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc)))

(define-testcase part0-nfc ()
  (loop for x in *part0* do (validate-nfc x)))

(define-testcase part1-nfc ()
  (loop for x in *part1* do (validate-nfc x)))

(define-testcase part2-nfc ()
  (loop for x in *part2* do (validate-nfc x)))

(define-testcase part3-nfc ()
  (loop for x in *part3* do (validate-nfc x)))

(define-testcase all-nfc ()
  (part0-nfc)
  (part1-nfc)
  (part2-nfc)
  (part3-nfc))

(define-testcase validate-nfd (x)
  (assert-equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd))
  (assert-equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd))
  (assert-equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd))
  (assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd))
  (assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd)))

(define-testcase part0-nfd ()
  (loop for x in *part0* do (validate-nfd x)))

(define-testcase part1-nfd ()
  (loop for x in *part1* do (validate-nfd x)))

(define-testcase part2-nfd ()
  (loop for x in *part2* do (validate-nfd x)))

(define-testcase part3-nfd ()
  (loop for x in *part3* do (validate-nfd x)))

(define-testcase all-nfd ()
  (part0-nfd)
  (part1-nfd)
  (part2-nfd)
  (part3-nfd))

(define-testcase every-unicode-letter-p (string)
  (loop for c across string do (assert-t (unicode-letter-p c))))

(define-testcase unicode-letters ()
  (every-unicode-letter-p "새우")
  (every-unicode-letter-p "이해")
  (assert-nil (every #'unicode-letter-p "zAp2"))
  (every-unicode-letter-p "タイムゾーン")
  (every-unicode-letter-p "タイムゾーン")
  (every-unicode-letter-p "时区")
  (every-unicode-letter-p "時區"))

(define-testcase run-all-tests ()
  (all-nfkc)
  (all-nfkd)
  (all-nfc)
  (all-nfd)
  (unicode-letters))
