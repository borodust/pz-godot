(common-lisp:in-package :%godot)


(defgmethod
 (xrbody-modifier-3d+set-body-tracker :class 'xrbody-modifier-3d :bind
  "set_body_tracker" :hash 3304788590)
 :void (tracker-name string-name))

(defgmethod
 (xrbody-modifier-3d+get-body-tracker :class 'xrbody-modifier-3d :bind
  "get_body_tracker" :hash 2002593661)
 string-name)

(defgmethod
 (xrbody-modifier-3d+set-body-update :class 'xrbody-modifier-3d :bind
  "set_body_update" :hash 2211199417)
 :void (body-update xrbody-modifier-3d+body-update))

(defgmethod
 (xrbody-modifier-3d+get-body-update :class 'xrbody-modifier-3d :bind
  "get_body_update" :hash 2642335328)
 xrbody-modifier-3d+body-update)

(defgmethod
 (xrbody-modifier-3d+set-bone-update :class 'xrbody-modifier-3d :bind
  "set_bone_update" :hash 3356796943)
 :void (bone-update xrbody-modifier-3d+bone-update))

(defgmethod
 (xrbody-modifier-3d+get-bone-update :class 'xrbody-modifier-3d :bind
  "get_bone_update" :hash 1309305964)
 xrbody-modifier-3d+bone-update)