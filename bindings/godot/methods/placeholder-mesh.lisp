(common-lisp:in-package :%godot)


(defgmethod
 (placeholder-mesh+set-aabb :class 'placeholder-mesh :bind "set_aabb" :hash
  259215842)
 :void (aabb aabb))