(common-lisp:in-package :%godot)


(defgproperty directional-light-3d+directional-shadow-mode
 'directional-light-3d :get 'directional-light-3d+get-shadow-mode :set
 'directional-light-3d+set-shadow-mode)

(defgproperty directional-light-3d+directional-shadow-split-1
 'directional-light-3d :index 10)

(defgproperty directional-light-3d+directional-shadow-split-2
 'directional-light-3d :index 11)

(defgproperty directional-light-3d+directional-shadow-split-3
 'directional-light-3d :index 12)

(defgproperty directional-light-3d+directional-shadow-blend-splits
 'directional-light-3d :get 'directional-light-3d+is-blend-splits-enabled :set
 'directional-light-3d+set-blend-splits)

(defgproperty directional-light-3d+directional-shadow-fade-start
 'directional-light-3d :index 13)

(defgproperty directional-light-3d+directional-shadow-max-distance
 'directional-light-3d :index 9)

(defgproperty directional-light-3d+directional-shadow-pancake-size
 'directional-light-3d :index 16)

(defgproperty directional-light-3d+sky-mode 'directional-light-3d :get
 'directional-light-3d+get-sky-mode :set 'directional-light-3d+set-sky-mode)