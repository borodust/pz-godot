(common-lisp:in-package :%godot)


(defgmethod
 (render-scene-buffers+configure :class 'render-scene-buffers :bind "configure"
  :hash 3072623270)
 :void (config render-scene-buffers-configuration))