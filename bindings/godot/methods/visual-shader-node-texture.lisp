(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-texture+set-source :class 'visual-shader-node-texture
  :bind "set_source" :hash 905262939)
 :void (value visual-shader-node-texture+source))

(defgmethod
 (visual-shader-node-texture+get-source :class 'visual-shader-node-texture
  :bind "get_source" :hash 2896297444)
 visual-shader-node-texture+source)

(defgmethod
 (visual-shader-node-texture+set-texture :class 'visual-shader-node-texture
  :bind "set_texture" :hash 4051416890)
 :void (value texture-2d))

(defgmethod
 (visual-shader-node-texture+get-texture :class 'visual-shader-node-texture
  :bind "get_texture" :hash 3635182373)
 texture-2d)

(defgmethod
 (visual-shader-node-texture+set-texture-type :class
  'visual-shader-node-texture :bind "set_texture_type" :hash 986314081)
 :void (value visual-shader-node-texture+texture-type))

(defgmethod
 (visual-shader-node-texture+get-texture-type :class
  'visual-shader-node-texture :bind "get_texture_type" :hash 3290430153)
 visual-shader-node-texture+texture-type)