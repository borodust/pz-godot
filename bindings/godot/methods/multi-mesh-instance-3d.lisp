(common-lisp:in-package :%godot)


(defgmethod
 (multi-mesh-instance-3d+set-multimesh :class 'multi-mesh-instance-3d :bind
  "set_multimesh" :hash 2246127404)
 :void (multimesh multi-mesh))

(defgmethod
 (multi-mesh-instance-3d+get-multimesh :class 'multi-mesh-instance-3d :bind
  "get_multimesh" :hash 1385450523)
 multi-mesh)