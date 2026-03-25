(asdf:defsystem :pz-godot-lib
  :description "Bindings to LibGodot API"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:cffi :pz-godot-gdext)
  :pathname "bindings/"
  :serial t
  :components ((:file "libgodot")))
