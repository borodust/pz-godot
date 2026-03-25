(common-lisp:in-package :%godot)


(defgmethod
 (root-motion-view+set-animation-path :class 'root-motion-view :bind
  "set_animation_path" :hash 1348162250)
 :void (path node-path))

(defgmethod
 (root-motion-view+get-animation-path :class 'root-motion-view :bind
  "get_animation_path" :hash 4075236667)
 node-path)

(defgmethod
 (root-motion-view+set-color :class 'root-motion-view :bind "set_color" :hash
  2920490490)
 :void (color color))

(defgmethod
 (root-motion-view+get-color :class 'root-motion-view :bind "get_color" :hash
  3444240500)
 color)

(defgmethod
 (root-motion-view+set-cell-size :class 'root-motion-view :bind "set_cell_size"
  :hash 373806689)
 :void (size float))

(defgmethod
 (root-motion-view+get-cell-size :class 'root-motion-view :bind "get_cell_size"
  :hash 1740695150)
 float)

(defgmethod
 (root-motion-view+set-radius :class 'root-motion-view :bind "set_radius" :hash
  373806689)
 :void (size float))

(defgmethod
 (root-motion-view+get-radius :class 'root-motion-view :bind "get_radius" :hash
  1740695150)
 float)

(defgmethod
 (root-motion-view+set-zero-y :class 'root-motion-view :bind "set_zero_y" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (root-motion-view+get-zero-y :class 'root-motion-view :bind "get_zero_y" :hash
  36873697)
 bool)