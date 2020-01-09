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

(defpackage :ruby-objects.system
  (:use :common-lisp :asdf))

(in-package :ruby-objects.system)

(defsystem :ruby-objects
  :name "ruby-objects"
  :author "Thomas de Grivel <thoxdg@gmail.com>"
  :version "0.1"
  :description "Ruby objects for Common Lisp"
  :components
  ((:file "package")
   (:file "ruby-class" :depends-on ("package"))
   (:file "ruby-object" :depends-on ("ruby-class"))
   (:file "class" :depends-on ("ruby-object"))
   (:file "object" :depends-on ("ruby-object"))
   (:file "test" :depends-on ("class" "object"))))
