(common-lisp:in-package :%godot)


(defgmethod
 (primitive-mesh+%create-mesh-array :class 'primitive-mesh :bind
  "_create_mesh_array" :hash 3995934104 :virtual common-lisp:t)
 array)

(defgmethod
 (primitive-mesh+set-material :class 'primitive-mesh :bind "set_material" :hash
  2757459619)
 :void (material material))

(defgmethod
 (primitive-mesh+get-material :class 'primitive-mesh :bind "get_material" :hash
  5934680)
 material)

(defgmethod
 (primitive-mesh+get-mesh-arrays :class 'primitive-mesh :bind "get_mesh_arrays"
  :hash 3995934104)
 array)

(defgmethod
 (primitive-mesh+set-custom-aabb :class 'primitive-mesh :bind "set_custom_aabb"
  :hash 259215842)
 :void (aabb aabb))

(defgmethod
 (primitive-mesh+get-custom-aabb :class 'primitive-mesh :bind "get_custom_aabb"
  :hash 1068685055)
 aabb)

(defgmethod
 (primitive-mesh+set-flip-faces :class 'primitive-mesh :bind "set_flip_faces"
  :hash 2586408642)
 :void (flip-faces bool))

(defgmethod
 (primitive-mesh+get-flip-faces :class 'primitive-mesh :bind "get_flip_faces"
  :hash 36873697)
 bool)

(defgmethod
 (primitive-mesh+set-add-uv2 :class 'primitive-mesh :bind "set_add_uv2" :hash
  2586408642)
 :void (add-uv2 bool))

(defgmethod
 (primitive-mesh+get-add-uv2 :class 'primitive-mesh :bind "get_add_uv2" :hash
  36873697)
 bool)

(defgmethod
 (primitive-mesh+set-uv2-padding :class 'primitive-mesh :bind "set_uv2_padding"
  :hash 373806689)
 :void (uv2-padding float))

(defgmethod
 (primitive-mesh+get-uv2-padding :class 'primitive-mesh :bind "get_uv2_padding"
  :hash 1740695150)
 float)

(defgmethod
 (primitive-mesh+request-update :class 'primitive-mesh :bind "request_update"
  :hash 3218959716)
 :void)