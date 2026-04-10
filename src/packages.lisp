(cl:defpackage :pz-godot
  (:use :cl)
  (:local-nicknames (:a :alexandria)
                    (:jzon :com.inuoe.jzon))
  (:export #:regenerate-bindings))


(cl:defpackage :%%pz-godot~gdext
  (:use)
  (:export defifun
           defcfunproto))

(cl:defpackage :%%pz-godot~godot
  (:use)
  (:export defgconstant
           defgenum
           defgclass
           defgconstructor
           defgdestructor
           defgproperty
           defgmethod
           defgsingleton))
