(common-lisp:in-package :%godot)


(defgmethod
 (plane-mesh+set-size :class 'plane-mesh :bind "set_size" :hash 743155724)
 :void (size vector-2))

(defgmethod
 (plane-mesh+get-size :class 'plane-mesh :bind "get_size" :hash 3341600327)
 vector-2)

(defgmethod
 (plane-mesh+set-subdivide-width :class 'plane-mesh :bind "set_subdivide_width"
  :hash 1286410249)
 :void (subdivide int))

(defgmethod
 (plane-mesh+get-subdivide-width :class 'plane-mesh :bind "get_subdivide_width"
  :hash 3905245786)
 int)

(defgmethod
 (plane-mesh+set-subdivide-depth :class 'plane-mesh :bind "set_subdivide_depth"
  :hash 1286410249)
 :void (subdivide int))

(defgmethod
 (plane-mesh+get-subdivide-depth :class 'plane-mesh :bind "get_subdivide_depth"
  :hash 3905245786)
 int)

(defgmethod
 (plane-mesh+set-center-offset :class 'plane-mesh :bind "set_center_offset"
  :hash 3460891852)
 :void (offset vector-3))

(defgmethod
 (plane-mesh+get-center-offset :class 'plane-mesh :bind "get_center_offset"
  :hash 3360562783)
 vector-3)

(defgmethod
 (plane-mesh+set-orientation :class 'plane-mesh :bind "set_orientation" :hash
  2751399687)
 :void (orientation plane-mesh+orientation))

(defgmethod
 (plane-mesh+get-orientation :class 'plane-mesh :bind "get_orientation" :hash
  3227599250)
 plane-mesh+orientation)