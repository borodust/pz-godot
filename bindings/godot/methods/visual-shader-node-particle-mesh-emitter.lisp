(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-particle-mesh-emitter+set-mesh :class
  'visual-shader-node-particle-mesh-emitter :bind "set_mesh" :hash 194775623)
 :void (mesh mesh))

(defgmethod
 (visual-shader-node-particle-mesh-emitter+get-mesh :class
  'visual-shader-node-particle-mesh-emitter :bind "get_mesh" :hash 1808005922)
 mesh)

(defgmethod
 (visual-shader-node-particle-mesh-emitter+set-use-all-surfaces :class
  'visual-shader-node-particle-mesh-emitter :bind "set_use_all_surfaces" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (visual-shader-node-particle-mesh-emitter+is-use-all-surfaces :class
  'visual-shader-node-particle-mesh-emitter :bind "is_use_all_surfaces" :hash
  36873697)
 bool)

(defgmethod
 (visual-shader-node-particle-mesh-emitter+set-surface-index :class
  'visual-shader-node-particle-mesh-emitter :bind "set_surface_index" :hash
  1286410249)
 :void (surface-index int))

(defgmethod
 (visual-shader-node-particle-mesh-emitter+get-surface-index :class
  'visual-shader-node-particle-mesh-emitter :bind "get_surface_index" :hash
  3905245786)
 int)