(common-lisp:in-package :%godot)


(defgmethod
 (external-texture+set-size :class 'external-texture :bind "set_size" :hash
  743155724)
 :void (size vector-2))

(defgmethod
 (external-texture+get-external-texture-id :class 'external-texture :bind
  "get_external_texture_id" :hash 3905245786)
 int)

(defgmethod
 (external-texture+set-external-buffer-id :class 'external-texture :bind
  "set_external_buffer_id" :hash 1286410249)
 :void (external-buffer-id int))