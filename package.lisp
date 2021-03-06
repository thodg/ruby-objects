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

(in-package :common-lisp-user)

(defpackage :ruby
  (:use :common-lisp)
  (:shadow
   #:class
   #:find-class
   #:find-method)
  (:export
   #:@
   #:*self*
   #:class
   #:def
   #:new
   #:private
   #:protected
   #:public
   #:public-send
   #:public-send*
   #:ruby-class
   #:ruby-object
   #:send
   #:send*
   #:shadowing-import-from))

(in-package :ruby)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from ()
    `(:shadowing-import-from :ruby
                             ,@(package-shadowing-symbols :ruby))))

(in-package :common-lisp-user)

(delete-package :ruby-user)

(defpackage :ruby-user
  (:use :common-lisp :ruby)
  #.(ruby:shadowing-import-from))
