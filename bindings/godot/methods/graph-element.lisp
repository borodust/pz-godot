(common-lisp:in-package :%godot)


(defgmethod
 (graph-element+set-resizable :class 'graph-element :bind "set_resizable" :hash
  2586408642)
 :void (resizable bool))

(defgmethod
 (graph-element+is-resizable :class 'graph-element :bind "is_resizable" :hash
  36873697)
 bool)

(defgmethod
 (graph-element+set-draggable :class 'graph-element :bind "set_draggable" :hash
  2586408642)
 :void (draggable bool))

(defgmethod
 (graph-element+is-draggable :class 'graph-element :bind "is_draggable" :hash
  2240911060)
 bool)

(defgmethod
 (graph-element+set-selectable :class 'graph-element :bind "set_selectable"
  :hash 2586408642)
 :void (selectable bool))

(defgmethod
 (graph-element+is-selectable :class 'graph-element :bind "is_selectable" :hash
  2240911060)
 bool)

(defgmethod
 (graph-element+set-selected :class 'graph-element :bind "set_selected" :hash
  2586408642)
 :void (selected bool))

(defgmethod
 (graph-element+is-selected :class 'graph-element :bind "is_selected" :hash
  2240911060)
 bool)

(defgmethod
 (graph-element+set-scaling-menus :class 'graph-element :bind
  "set_scaling_menus" :hash 2586408642)
 :void (scaling-menus bool))

(defgmethod
 (graph-element+is-scaling-menus :class 'graph-element :bind "is_scaling_menus"
  :hash 36873697)
 bool)

(defgmethod
 (graph-element+set-position-offset :class 'graph-element :bind
  "set_position_offset" :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (graph-element+get-position-offset :class 'graph-element :bind
  "get_position_offset" :hash 3341600327)
 vector-2)