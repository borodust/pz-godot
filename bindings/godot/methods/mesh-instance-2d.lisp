(common-lisp:in-package :%godot)


(defgmethod
 (mesh-instance-2d+set-mesh :class 'mesh-instance-2d :bind "set_mesh" :hash
  194775623)
 :void (mesh mesh))

(defgmethod
 (mesh-instance-2d+get-mesh :class 'mesh-instance-2d :bind "get_mesh" :hash
  1808005922)
 mesh)

(defgmethod
 (mesh-instance-2d+set-texture :class 'mesh-instance-2d :bind "set_texture"
  :hash 4051416890)
 :void (texture texture-2d))

(defgmethod
 (mesh-instance-2d+get-texture :class 'mesh-instance-2d :bind "get_texture"
  :hash 3635182373)
 texture-2d)