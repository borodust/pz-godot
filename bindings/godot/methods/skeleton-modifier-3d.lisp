(common-lisp:in-package :%godot)


(defgmethod
 (skeleton-modifier-3d+-process-modification-with-delta :class
  'skeleton-modifier-3d :bind "_process_modification_with_delta" :hash
  373806689 :virtual common-lisp:t)
 :void (delta float))

(defgmethod
 (skeleton-modifier-3d+-process-modification :class 'skeleton-modifier-3d :bind
  "_process_modification" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (skeleton-modifier-3d+-skeleton-changed :class 'skeleton-modifier-3d :bind
  "_skeleton_changed" :hash 2926744397 :virtual common-lisp:t)
 :void (old-skeleton skeleton-3d) (new-skeleton skeleton-3d))

(defgmethod
 (skeleton-modifier-3d+-validate-bone-names :class 'skeleton-modifier-3d :bind
  "_validate_bone_names" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (skeleton-modifier-3d+get-skeleton :class 'skeleton-modifier-3d :bind
  "get_skeleton" :hash 1488626673)
 skeleton-3d)

(defgmethod
 (skeleton-modifier-3d+set-active :class 'skeleton-modifier-3d :bind
  "set_active" :hash 2586408642)
 :void (active bool))

(defgmethod
 (skeleton-modifier-3d+is-active :class 'skeleton-modifier-3d :bind "is_active"
  :hash 36873697)
 bool)

(defgmethod
 (skeleton-modifier-3d+set-influence :class 'skeleton-modifier-3d :bind
  "set_influence" :hash 373806689)
 :void (influence float))

(defgmethod
 (skeleton-modifier-3d+get-influence :class 'skeleton-modifier-3d :bind
  "get_influence" :hash 1740695150)
 float)