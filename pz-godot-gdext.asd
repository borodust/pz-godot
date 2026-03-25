(asdf:defsystem :pz-godot-gdext
  :description "Bindings to GDExtension API"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:cffi :pz-godot/common)
  :pathname "bindings/gdext/"
  :serial t
  :components ((:file "packages") (:file "types") (:file "interface")))