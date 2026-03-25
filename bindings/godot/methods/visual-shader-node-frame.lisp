(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-frame+set-title :class 'visual-shader-node-frame :bind
  "set_title" :hash 83702148)
 :void (title string))

(defgmethod
 (visual-shader-node-frame+get-title :class 'visual-shader-node-frame :bind
  "get_title" :hash 201670096)
 string)

(defgmethod
 (visual-shader-node-frame+set-tint-color-enabled :class
  'visual-shader-node-frame :bind "set_tint_color_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (visual-shader-node-frame+is-tint-color-enabled :class
  'visual-shader-node-frame :bind "is_tint_color_enabled" :hash 36873697)
 bool)

(defgmethod
 (visual-shader-node-frame+set-tint-color :class 'visual-shader-node-frame
  :bind "set_tint_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (visual-shader-node-frame+get-tint-color :class 'visual-shader-node-frame
  :bind "get_tint_color" :hash 3444240500)
 color)

(defgmethod
 (visual-shader-node-frame+set-autoshrink-enabled :class
  'visual-shader-node-frame :bind "set_autoshrink_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (visual-shader-node-frame+is-autoshrink-enabled :class
  'visual-shader-node-frame :bind "is_autoshrink_enabled" :hash 36873697)
 bool)

(defgmethod
 (visual-shader-node-frame+add-attached-node :class 'visual-shader-node-frame
  :bind "add_attached_node" :hash 1286410249)
 :void (node int))

(defgmethod
 (visual-shader-node-frame+remove-attached-node :class
  'visual-shader-node-frame :bind "remove_attached_node" :hash 1286410249)
 :void (node int))

(defgmethod
 (visual-shader-node-frame+set-attached-nodes :class 'visual-shader-node-frame
  :bind "set_attached_nodes" :hash 3614634198)
 :void (attached-nodes packed-int-32array))

(defgmethod
 (visual-shader-node-frame+get-attached-nodes :class 'visual-shader-node-frame
  :bind "get_attached_nodes" :hash 1930428628)
 packed-int-32array)