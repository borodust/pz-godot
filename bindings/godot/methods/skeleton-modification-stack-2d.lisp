(common-lisp:in-package :%godot)


(defgmethod
 (skeleton-modification-stack-2d+setup :class 'skeleton-modification-stack-2d
  :bind "setup" :hash 3218959716)
 :void)

(defgmethod
 (skeleton-modification-stack-2d+execute :class 'skeleton-modification-stack-2d
  :bind "execute" :hash 1005356550)
 :void (delta float) (execution-mode int))

(defgmethod
 (skeleton-modification-stack-2d+enable-all-modifications :class
  'skeleton-modification-stack-2d :bind "enable_all_modifications" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (skeleton-modification-stack-2d+get-modification :class
  'skeleton-modification-stack-2d :bind "get_modification" :hash 2570274329)
 skeleton-modification-2d (mod-idx int))

(defgmethod
 (skeleton-modification-stack-2d+add-modification :class
  'skeleton-modification-stack-2d :bind "add_modification" :hash 354162120)
 :void (modification skeleton-modification-2d))

(defgmethod
 (skeleton-modification-stack-2d+delete-modification :class
  'skeleton-modification-stack-2d :bind "delete_modification" :hash 1286410249)
 :void (mod-idx int))

(defgmethod
 (skeleton-modification-stack-2d+set-modification :class
  'skeleton-modification-stack-2d :bind "set_modification" :hash 1098262544)
 :void (mod-idx int) (modification skeleton-modification-2d))

(defgmethod
 (skeleton-modification-stack-2d+set-modification-count :class
  'skeleton-modification-stack-2d :bind "set_modification_count" :hash
  1286410249)
 :void (count int))

(defgmethod
 (skeleton-modification-stack-2d+get-modification-count :class
  'skeleton-modification-stack-2d :bind "get_modification_count" :hash
  3905245786)
 int)

(defgmethod
 (skeleton-modification-stack-2d+get-is-setup :class
  'skeleton-modification-stack-2d :bind "get_is_setup" :hash 36873697)
 bool)

(defgmethod
 (skeleton-modification-stack-2d+set-enabled :class
  'skeleton-modification-stack-2d :bind "set_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (skeleton-modification-stack-2d+get-enabled :class
  'skeleton-modification-stack-2d :bind "get_enabled" :hash 36873697)
 bool)

(defgmethod
 (skeleton-modification-stack-2d+set-strength :class
  'skeleton-modification-stack-2d :bind "set_strength" :hash 373806689)
 :void (strength float))

(defgmethod
 (skeleton-modification-stack-2d+get-strength :class
  'skeleton-modification-stack-2d :bind "get_strength" :hash 1740695150)
 float)

(defgmethod
 (skeleton-modification-stack-2d+get-skeleton :class
  'skeleton-modification-stack-2d :bind "get_skeleton" :hash 1697361217)
 skeleton-2d)