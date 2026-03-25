(common-lisp:in-package :%godot)


(defgmethod
 (multi-mesh-instance-2d+set-multimesh :class 'multi-mesh-instance-2d :bind
  "set_multimesh" :hash 2246127404)
 :void (multimesh multi-mesh))

(defgmethod
 (multi-mesh-instance-2d+get-multimesh :class 'multi-mesh-instance-2d :bind
  "get_multimesh" :hash 1385450523)
 multi-mesh)

(defgmethod
 (multi-mesh-instance-2d+set-texture :class 'multi-mesh-instance-2d :bind
  "set_texture" :hash 4051416890)
 :void (texture texture-2d))

(defgmethod
 (multi-mesh-instance-2d+get-texture :class 'multi-mesh-instance-2d :bind
  "get_texture" :hash 3635182373)
 texture-2d)