(in-package "COMMON-LISP-USER")

(defpackage #:parser
  (:use #:common-lisp))

(defpackage #:ebnf
  (:depends-on )
  (:use #:common-lisp #:cl-ppcre #:trivial-utf-8)
  (:export #:enable-ebnf-syntax
           #:disable-ebnf-syntax))

(defpackage #:go
  (:use #:common-lisp #:ebnf))
