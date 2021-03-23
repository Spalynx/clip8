;;;; clip8.asd

(asdf:defsystem #:clip8
  :description "A Chip-8 Emulator written in LISP!"
  :author "Mark Eikel <Spalynx@gmail.com>"
  :license  "The Unlicense"
  :version "0.9.0"
  :serial t
  :entry-point "clip8:start"
  :depends-on (#:alexandria #:ltk)
  :components ((:file "package")
               (:file "src/util")
               (:file "src/opcodes" :depends-on ("src/util"))
               (:file "src/cpu" :depends-on ("src/opcodes" "src/util"))
               (:file "src/gui")
               (:file "clip8")))
