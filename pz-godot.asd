(asdf:defsystem :pz-godot
  :description "Bindings to Godot game engine"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:pz-godot-gdext :pz-godot-lib :pz-godot-api))


(asdf:defsystem :pz-godot/ext
  :description "Utilities to use with Godot extension API"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:alexandria :uiop :cffi :cffi-c-ref :pz-godot/common :pz-godot-gdext)
  :pathname "bindings/"
  :components ((:file "ext")))


(asdf:defsystem :pz-godot/common
  :description "Common utilities to use with the bindings"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:alexandria :uiop :cffi :cffi-c-ref)
  :pathname "bindings/"
  :components ((:file "common")))


(asdf:defsystem :pz-godot/wrapper
  :description "Bindings generator for GDExtension API and Godot extensions"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:uiop :alexandria
               :cl-ppcre :cffi
               :com.inuoe.jzon :cl-fad
               :pz-godot/common)
  :serial t
  :pathname "src/"
  :components ((:file "packages")
               (:file "clawless")))


(asdf:defsystem :pz-godot/example
  :description "PZ-GODOT example"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:float-features :trivial-main-thread :cffi :pz-godot)
  :serial t
  :pathname "example/"
  :components ((:file "example")))
