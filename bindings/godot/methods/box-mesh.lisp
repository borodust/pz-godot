(common-lisp:in-package :%godot)


(defgmethod
 (box-mesh+set-size :class 'box-mesh :bind "set_size" :hash 3460891852) :void
 (size vector-3))

(defgmethod
 (box-mesh+get-size :class 'box-mesh :bind "get_size" :hash 3360562783)
 vector-3)

(defgmethod
 (box-mesh+set-subdivide-width :class 'box-mesh :bind "set_subdivide_width"
  :hash 1286410249)
 :void (subdivide int))

(defgmethod
 (box-mesh+get-subdivide-width :class 'box-mesh :bind "get_subdivide_width"
  :hash 3905245786)
 int)

(defgmethod
 (box-mesh+set-subdivide-height :class 'box-mesh :bind "set_subdivide_height"
  :hash 1286410249)
 :void (divisions int))

(defgmethod
 (box-mesh+get-subdivide-height :class 'box-mesh :bind "get_subdivide_height"
  :hash 3905245786)
 int)

(defgmethod
 (box-mesh+set-subdivide-depth :class 'box-mesh :bind "set_subdivide_depth"
  :hash 1286410249)
 :void (divisions int))

(defgmethod
 (box-mesh+get-subdivide-depth :class 'box-mesh :bind "get_subdivide_depth"
  :hash 3905245786)
 int)