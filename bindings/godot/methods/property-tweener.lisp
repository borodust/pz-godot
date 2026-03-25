(common-lisp:in-package :%godot)


(defgmethod
 (property-tweener+from :class 'property-tweener :bind "from" :hash 4190193059)
 property-tweener (value variant))

(defgmethod
 (property-tweener+from-current :class 'property-tweener :bind "from_current"
  :hash 4279177709)
 property-tweener)

(defgmethod
 (property-tweener+as-relative :class 'property-tweener :bind "as_relative"
  :hash 4279177709)
 property-tweener)

(defgmethod
 (property-tweener+set-trans :class 'property-tweener :bind "set_trans" :hash
  1899107404)
 property-tweener (trans tween+transition-type))

(defgmethod
 (property-tweener+set-ease :class 'property-tweener :bind "set_ease" :hash
  1080455622)
 property-tweener (ease tween+ease-type))

(defgmethod
 (property-tweener+set-custom-interpolator :class 'property-tweener :bind
  "set_custom_interpolator" :hash 3174170268)
 property-tweener (interpolator-method callable))

(defgmethod
 (property-tweener+set-delay :class 'property-tweener :bind "set_delay" :hash
  2171559331)
 property-tweener (delay float))