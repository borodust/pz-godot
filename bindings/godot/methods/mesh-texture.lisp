(common-lisp:in-package :%godot)


(defgmethod
 (mesh-texture+set-mesh :class 'mesh-texture :bind "set_mesh" :hash 194775623)
 :void (mesh mesh))

(defgmethod
 (mesh-texture+get-mesh :class 'mesh-texture :bind "get_mesh" :hash 1808005922)
 mesh)

(defgmethod
 (mesh-texture+set-image-size :class 'mesh-texture :bind "set_image_size" :hash
  743155724)
 :void (size vector-2))

(defgmethod
 (mesh-texture+get-image-size :class 'mesh-texture :bind "get_image_size" :hash
  3341600327)
 vector-2)

(defgmethod
 (mesh-texture+set-base-texture :class 'mesh-texture :bind "set_base_texture"
  :hash 4051416890)
 :void (texture texture-2d))

(defgmethod
 (mesh-texture+get-base-texture :class 'mesh-texture :bind "get_base_texture"
  :hash 3635182373)
 texture-2d)