(common-lisp:in-package :%godot)


(defgmethod
 (rdacceleration-structure-instance+set-transform :class
  'rdacceleration-structure-instance :bind "set_transform" :hash 2952846383)
 :void (p-member transform-3d))

(defgmethod
 (rdacceleration-structure-instance+get-transform :class
  'rdacceleration-structure-instance :bind "get_transform" :hash 3229777777)
 transform-3d)

(defgmethod
 (rdacceleration-structure-instance+set-id :class
  'rdacceleration-structure-instance :bind "set_id" :hash 1286410249)
 :void (p-member int))

(defgmethod
 (rdacceleration-structure-instance+get-id :class
  'rdacceleration-structure-instance :bind "get_id" :hash 3905245786)
 int)

(defgmethod
 (rdacceleration-structure-instance+set-mask :class
  'rdacceleration-structure-instance :bind "set_mask" :hash 1286410249)
 :void (p-member int))

(defgmethod
 (rdacceleration-structure-instance+get-mask :class
  'rdacceleration-structure-instance :bind "get_mask" :hash 3905245786)
 int)

(defgmethod
 (rdacceleration-structure-instance+set-hit-sbt-range :class
  'rdacceleration-structure-instance :bind "set_hit_sbt_range" :hash
  1286410249)
 :void (p-member int))

(defgmethod
 (rdacceleration-structure-instance+get-hit-sbt-range :class
  'rdacceleration-structure-instance :bind "get_hit_sbt_range" :hash
  3905245786)
 int)

(defgmethod
 (rdacceleration-structure-instance+set-flags :class
  'rdacceleration-structure-instance :bind "set_flags" :hash 2971840141)
 :void (p-member rendering-device+acceleration-structure-instance-flag-bits))

(defgmethod
 (rdacceleration-structure-instance+get-flags :class
  'rdacceleration-structure-instance :bind "get_flags" :hash 2410182637)
 rendering-device+acceleration-structure-instance-flag-bits)

(defgmethod
 (rdacceleration-structure-instance+set-blas :class
  'rdacceleration-structure-instance :bind "set_blas" :hash 2722037293)
 :void (p-member rid))

(defgmethod
 (rdacceleration-structure-instance+get-blas :class
  'rdacceleration-structure-instance :bind "get_blas" :hash 2944877500)
 rid)