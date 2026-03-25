(common-lisp:in-package :%godot)


(defgmethod
 (xrhand-modifier-3d+set-hand-tracker :class 'xrhand-modifier-3d :bind
  "set_hand_tracker" :hash 3304788590)
 :void (tracker-name string-name))

(defgmethod
 (xrhand-modifier-3d+get-hand-tracker :class 'xrhand-modifier-3d :bind
  "get_hand_tracker" :hash 2002593661)
 string-name)

(defgmethod
 (xrhand-modifier-3d+set-bone-update :class 'xrhand-modifier-3d :bind
  "set_bone_update" :hash 3635701455)
 :void (bone-update xrhand-modifier-3d+bone-update))

(defgmethod
 (xrhand-modifier-3d+get-bone-update :class 'xrhand-modifier-3d :bind
  "get_bone_update" :hash 2873665691)
 xrhand-modifier-3d+bone-update)