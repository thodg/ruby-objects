# Ruby objects for Common Lisp

```
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
  (format t "~&test fact 5 = ~A~%" (send test :fact 5))
  (format t "test x ~A~%" (send test :x))
  (format t "test x= 5 ~A~%" (send test :x= 5))
  (format t "test x ~A~%" (send test :x)))
```

gives

```
test fact 5 = 120
test x NIL
test x= 5 5
test x 5
```
