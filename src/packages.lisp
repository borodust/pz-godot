(cl:defpackage :pz-godot
  (:use :cl)
  (:local-nicknames (:a :alexandria)
                    (:jzon :com.inuoe.jzon))
  (:export #:regenerate-bindings))

(cl:defpackage :pz-godot-pristine
  (:use))
