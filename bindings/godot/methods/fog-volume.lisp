(common-lisp:in-package :%godot)


(defgmethod
 (fog-volume+set-size :class 'fog-volume :bind "set_size" :hash 3460891852)
 :void (size vector-3))

(defgmethod
 (fog-volume+get-size :class 'fog-volume :bind "get_size" :hash 3360562783)
 vector-3)

(defgmethod
 (fog-volume+set-shape :class 'fog-volume :bind "set_shape" :hash 1416323362)
 :void (shape rendering-server+fog-volume-shape))

(defgmethod
 (fog-volume+get-shape :class 'fog-volume :bind "get_shape" :hash 3920334604)
 rendering-server+fog-volume-shape)

(defgmethod
 (fog-volume+set-material :class 'fog-volume :bind "set_material" :hash
  2757459619)
 :void (material material))

(defgmethod
 (fog-volume+get-material :class 'fog-volume :bind "get_material" :hash
  5934680)
 material)