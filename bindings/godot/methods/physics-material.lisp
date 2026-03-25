(common-lisp:in-package :%godot)


(defgmethod
 (physics-material+set-friction :class 'physics-material :bind "set_friction"
  :hash 373806689)
 :void (friction float))

(defgmethod
 (physics-material+get-friction :class 'physics-material :bind "get_friction"
  :hash 1740695150)
 float)

(defgmethod
 (physics-material+set-rough :class 'physics-material :bind "set_rough" :hash
  2586408642)
 :void (rough bool))

(defgmethod
 (physics-material+is-rough :class 'physics-material :bind "is_rough" :hash
  36873697)
 bool)

(defgmethod
 (physics-material+set-bounce :class 'physics-material :bind "set_bounce" :hash
  373806689)
 :void (bounce float))

(defgmethod
 (physics-material+get-bounce :class 'physics-material :bind "get_bounce" :hash
  1740695150)
 float)

(defgmethod
 (physics-material+set-absorbent :class 'physics-material :bind "set_absorbent"
  :hash 2586408642)
 :void (absorbent bool))

(defgmethod
 (physics-material+is-absorbent :class 'physics-material :bind "is_absorbent"
  :hash 36873697)
 bool)