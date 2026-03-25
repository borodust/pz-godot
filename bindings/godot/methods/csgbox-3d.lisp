(common-lisp:in-package :%godot)


(defgmethod
 (csgbox-3d+set-size :class 'csgbox-3d :bind "set_size" :hash 3460891852) :void
 (size vector-3))

(defgmethod
 (csgbox-3d+get-size :class 'csgbox-3d :bind "get_size" :hash 3360562783)
 vector-3)

(defgmethod
 (csgbox-3d+set-material :class 'csgbox-3d :bind "set_material" :hash
  2757459619)
 :void (material material))

(defgmethod
 (csgbox-3d+get-material :class 'csgbox-3d :bind "get_material" :hash 5934680)
 material)