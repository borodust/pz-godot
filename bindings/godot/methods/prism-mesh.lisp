(common-lisp:in-package :%godot)


(defgmethod
 (prism-mesh+set-left-to-right :class 'prism-mesh :bind "set_left_to_right"
  :hash 373806689)
 :void (left-to-right float))

(defgmethod
 (prism-mesh+get-left-to-right :class 'prism-mesh :bind "get_left_to_right"
  :hash 1740695150)
 float)

(defgmethod
 (prism-mesh+set-size :class 'prism-mesh :bind "set_size" :hash 3460891852)
 :void (size vector-3))

(defgmethod
 (prism-mesh+get-size :class 'prism-mesh :bind "get_size" :hash 3360562783)
 vector-3)

(defgmethod
 (prism-mesh+set-subdivide-width :class 'prism-mesh :bind "set_subdivide_width"
  :hash 1286410249)
 :void (segments int))

(defgmethod
 (prism-mesh+get-subdivide-width :class 'prism-mesh :bind "get_subdivide_width"
  :hash 3905245786)
 int)

(defgmethod
 (prism-mesh+set-subdivide-height :class 'prism-mesh :bind
  "set_subdivide_height" :hash 1286410249)
 :void (segments int))

(defgmethod
 (prism-mesh+get-subdivide-height :class 'prism-mesh :bind
  "get_subdivide_height" :hash 3905245786)
 int)

(defgmethod
 (prism-mesh+set-subdivide-depth :class 'prism-mesh :bind "set_subdivide_depth"
  :hash 1286410249)
 :void (segments int))

(defgmethod
 (prism-mesh+get-subdivide-depth :class 'prism-mesh :bind "get_subdivide_depth"
  :hash 3905245786)
 int)