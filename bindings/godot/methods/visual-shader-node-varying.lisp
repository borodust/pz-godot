(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-varying+set-varying-name :class
  'visual-shader-node-varying :bind "set_varying_name" :hash 83702148)
 :void (name string))

(defgmethod
 (visual-shader-node-varying+get-varying-name :class
  'visual-shader-node-varying :bind "get_varying_name" :hash 201670096)
 string)

(defgmethod
 (visual-shader-node-varying+set-varying-type :class
  'visual-shader-node-varying :bind "set_varying_type" :hash 3565867981)
 :void (type visual-shader+varying-type))

(defgmethod
 (visual-shader-node-varying+get-varying-type :class
  'visual-shader-node-varying :bind "get_varying_type" :hash 523183580)
 visual-shader+varying-type)