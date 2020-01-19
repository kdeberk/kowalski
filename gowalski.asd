(asdf:defsystem "gowalski"
  :description "A simple tool for analyzing go code."
  :depends-on (:cl-ppcre :trivial-utf-8)
  :version "0.0.1"
  :author "Kevin de Berk <kevin@dberk.nl>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "ebnf")))
