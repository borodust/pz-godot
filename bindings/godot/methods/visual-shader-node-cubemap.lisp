(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-cubemap+set-source :class 'visual-shader-node-cubemap
  :bind "set_source" :hash 1625400621)
 :void (value visual-shader-node-cubemap+source))

(defgmethod
 (visual-shader-node-cubemap+get-source :class 'visual-shader-node-cubemap
  :bind "get_source" :hash 2222048781)
 visual-shader-node-cubemap+source)

(defgmethod
 (visual-shader-node-cubemap+set-cube-map :class 'visual-shader-node-cubemap
  :bind "set_cube_map" :hash 1278366092)
 :void (value texture-layered))

(defgmethod
 (visual-shader-node-cubemap+get-cube-map :class 'visual-shader-node-cubemap
  :bind "get_cube_map" :hash 3984243839)
 texture-layered)

(defgmethod
 (visual-shader-node-cubemap+set-texture-type :class
  'visual-shader-node-cubemap :bind "set_texture_type" :hash 1899718876)
 :void (value visual-shader-node-cubemap+texture-type))

(defgmethod
 (visual-shader-node-cubemap+get-texture-type :class
  'visual-shader-node-cubemap :bind "get_texture_type" :hash 3356498888)
 visual-shader-node-cubemap+texture-type)