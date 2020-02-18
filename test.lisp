;;
;;  ruby-objects  -  Ruby objects for Common Lisp
;;
;;  Copyright 2019,2020 Thomas de Grivel <thoxdg@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :ruby-user)

(class test
  (private)
  (def fact (x)
    "This is a recursive implementation of factorial (!)"
    (declare (type integer x))
    (if (< x 1)
        1
        (* x (the integer (send* :fact (1- x))))))
  (def x= (x)
    "setter for x"
    (setf (@ x) x))
  (def x ()
    "getter for x"
    (@ x)))

(let ((test (new :test)))
  (format t "~&test fact 5 => ~A~%" (send test :fact 5))
  (format t "test x      => ~A~%" (send test :x))
  (format t "test x= 5   => ~A~%" (send test :x= 5))
  (format t "test x      => ~A~%" (send test :x)))

(class my-class

       (def my-method ()
           0)

       (def my-method1 (x)
         (1+ x))

       (def my-method2 (x y)
         (+ x y)))

(defvar *object* (new :my-class))
(defparameter *n* 4096)
(declaim (type fixnum *n*))

(defun test-public-send ()
  (time
   (let ((n *n*))
     (dotimes (x n)
       (dotimes (y n)
         (public-send *object* :my-method)
         (public-send *object* :my-method1 x)
         (public-send *object* :my-method2 x y))))))

(test-public-send)

(defun test-send ()
  (time
   (let ((n *n*))
     (dotimes (x n)
       (dotimes (y n)
         (send *object* :my-method)
         (send *object* :my-method1 x)
         (send *object* :my-method2 x y))))))

(test-send)
