(common-lisp:in-package :%godot)


(defgmethod
 (canvas-layer+set-layer :class 'canvas-layer :bind "set_layer" :hash
  1286410249)
 :void (layer int))

(defgmethod
 (canvas-layer+get-layer :class 'canvas-layer :bind "get_layer" :hash
  3905245786)
 int)

(defgmethod
 (canvas-layer+set-visible :class 'canvas-layer :bind "set_visible" :hash
  2586408642)
 :void (visible bool))

(defgmethod
 (canvas-layer+is-visible :class 'canvas-layer :bind "is_visible" :hash
  36873697)
 bool)

(defgmethod
 (canvas-layer+show :class 'canvas-layer :bind "show" :hash 3218959716) :void)

(defgmethod
 (canvas-layer+hide :class 'canvas-layer :bind "hide" :hash 3218959716) :void)

(defgmethod
 (canvas-layer+set-transform :class 'canvas-layer :bind "set_transform" :hash
  2761652528)
 :void (transform transform-2d))

(defgmethod
 (canvas-layer+get-transform :class 'canvas-layer :bind "get_transform" :hash
  3814499831)
 transform-2d)

(defgmethod
 (canvas-layer+get-final-transform :class 'canvas-layer :bind
  "get_final_transform" :hash 3814499831)
 transform-2d)

(defgmethod
 (canvas-layer+set-offset :class 'canvas-layer :bind "set_offset" :hash
  743155724)
 :void (offset vector-2))

(defgmethod
 (canvas-layer+get-offset :class 'canvas-layer :bind "get_offset" :hash
  3341600327)
 vector-2)

(defgmethod
 (canvas-layer+set-rotation :class 'canvas-layer :bind "set_rotation" :hash
  373806689)
 :void (radians float))

(defgmethod
 (canvas-layer+get-rotation :class 'canvas-layer :bind "get_rotation" :hash
  1740695150)
 float)

(defgmethod
 (canvas-layer+set-scale :class 'canvas-layer :bind "set_scale" :hash
  743155724)
 :void (scale vector-2))

(defgmethod
 (canvas-layer+get-scale :class 'canvas-layer :bind "get_scale" :hash
  3341600327)
 vector-2)

(defgmethod
 (canvas-layer+set-follow-viewport :class 'canvas-layer :bind
  "set_follow_viewport" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (canvas-layer+is-following-viewport :class 'canvas-layer :bind
  "is_following_viewport" :hash 36873697)
 bool)

(defgmethod
 (canvas-layer+set-follow-viewport-scale :class 'canvas-layer :bind
  "set_follow_viewport_scale" :hash 373806689)
 :void (scale float))

(defgmethod
 (canvas-layer+get-follow-viewport-scale :class 'canvas-layer :bind
  "get_follow_viewport_scale" :hash 1740695150)
 float)

(defgmethod
 (canvas-layer+set-custom-viewport :class 'canvas-layer :bind
  "set_custom_viewport" :hash 1078189570)
 :void (viewport node))

(defgmethod
 (canvas-layer+get-custom-viewport :class 'canvas-layer :bind
  "get_custom_viewport" :hash 3160264692)
 node)

(defgmethod
 (canvas-layer+get-canvas :class 'canvas-layer :bind "get_canvas" :hash
  2944877500)
 rid)