(common-lisp:in-package :%godot)


(defgproperty touch-screen-button+texture-normal 'touch-screen-button :get
 'touch-screen-button+get-texture-normal :set
 'touch-screen-button+set-texture-normal)

(defgproperty touch-screen-button+texture-pressed 'touch-screen-button :get
 'touch-screen-button+get-texture-pressed :set
 'touch-screen-button+set-texture-pressed)

(defgproperty touch-screen-button+bitmask 'touch-screen-button :get
 'touch-screen-button+get-bitmask :set 'touch-screen-button+set-bitmask)

(defgproperty touch-screen-button+shape 'touch-screen-button :get
 'touch-screen-button+get-shape :set 'touch-screen-button+set-shape)

(defgproperty touch-screen-button+shape-centered 'touch-screen-button :get
 'touch-screen-button+is-shape-centered :set
 'touch-screen-button+set-shape-centered)

(defgproperty touch-screen-button+shape-visible 'touch-screen-button :get
 'touch-screen-button+is-shape-visible :set
 'touch-screen-button+set-shape-visible)

(defgproperty touch-screen-button+passby-press 'touch-screen-button :get
 'touch-screen-button+is-passby-press-enabled :set
 'touch-screen-button+set-passby-press)

(defgproperty touch-screen-button+action 'touch-screen-button :get
 'touch-screen-button+get-action :set 'touch-screen-button+set-action)

(defgproperty touch-screen-button+visibility-mode 'touch-screen-button :get
 'touch-screen-button+get-visibility-mode :set
 'touch-screen-button+set-visibility-mode)