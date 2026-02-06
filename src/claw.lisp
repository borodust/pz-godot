(cl:defpackage :pz-godot
  (:use :cl :claw-utils)
  (:export))
(cl:in-package :pz-godot)


(claw.wrapper:defwrapper (:pz-godot
                          (:system :pz-godot/wrapper)
                          (:headers "gdextension_interface.gen.h"
                                    "libgodot.h")
                          (:includes :wrapper-includes
                                     :godot-extension-includes)
                          (:include-definitions "^GDExtension"
                                                "^GDEXTENSION_"
                                                "libgodot_")
                          (:targets ((:and :x86-64 :linux) "x86_64-pc-linux-gnu"))
                          (:persistent t :depends-on (:claw-utils))
                          (:language :c))
  :in-package :%gdext
  :trim-enum-prefix t
  :recognize-bitfields t
  :recognize-strings t
  :override-types ((:string claw-string)
                   (:pointer claw-pointer)
                   (:function-prototype-pointer claw-function-prototype-pointer))
  :with-adapter (:static
                 :path "src/lib/adapter.c")
  :symbolicate-names (:by-removing-prefixes "GDEXTENSION_"
                                            "GDExtension"
                                            "libgodot_"))
