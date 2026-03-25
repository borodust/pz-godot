(common-lisp:in-package :%godot)


(defgproperty editor-interface+distraction-free-mode 'editor-interface :get
 'editor-interface+is-distraction-free-mode-enabled :set
 'editor-interface+set-distraction-free-mode)

(defgproperty editor-interface+movie-maker-enabled 'editor-interface :get
 'editor-interface+is-movie-maker-enabled :set
 'editor-interface+set-movie-maker-enabled)