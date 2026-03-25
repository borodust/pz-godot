(common-lisp:in-package :%godot)


(defgmethod
 (gltflight+from-node :class 'gltflight :bind "from_node" :hash 3907677874
  :static common-lisp:t)
 gltflight (light-node light-3d))

(defgmethod
 (gltflight+to-node :class 'gltflight :bind "to_node" :hash 2040811672)
 light-3d)

(defgmethod
 (gltflight+from-dictionary :class 'gltflight :bind "from_dictionary" :hash
  4057087208 :static common-lisp:t)
 gltflight (dictionary dictionary))

(defgmethod
 (gltflight+to-dictionary :class 'gltflight :bind "to_dictionary" :hash
  3102165223)
 dictionary)

(defgmethod
 (gltflight+get-color :class 'gltflight :bind "get_color" :hash 3200896285)
 color)

(defgmethod
 (gltflight+set-color :class 'gltflight :bind "set_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (gltflight+get-intensity :class 'gltflight :bind "get_intensity" :hash
  191475506)
 float)

(defgmethod
 (gltflight+set-intensity :class 'gltflight :bind "set_intensity" :hash
  373806689)
 :void (intensity float))

(defgmethod
 (gltflight+get-light-type :class 'gltflight :bind "get_light_type" :hash
  2841200299)
 string)

(defgmethod
 (gltflight+set-light-type :class 'gltflight :bind "set_light_type" :hash
  83702148)
 :void (light-type string))

(defgmethod
 (gltflight+get-range :class 'gltflight :bind "get_range" :hash 191475506)
 float)

(defgmethod
 (gltflight+set-range :class 'gltflight :bind "set_range" :hash 373806689)
 :void (range float))

(defgmethod
 (gltflight+get-inner-cone-angle :class 'gltflight :bind "get_inner_cone_angle"
  :hash 191475506)
 float)

(defgmethod
 (gltflight+set-inner-cone-angle :class 'gltflight :bind "set_inner_cone_angle"
  :hash 373806689)
 :void (inner-cone-angle float))

(defgmethod
 (gltflight+get-outer-cone-angle :class 'gltflight :bind "get_outer_cone_angle"
  :hash 191475506)
 float)

(defgmethod
 (gltflight+set-outer-cone-angle :class 'gltflight :bind "set_outer_cone_angle"
  :hash 373806689)
 :void (outer-cone-angle float))

(defgmethod
 (gltflight+get-additional-data :class 'gltflight :bind "get_additional_data"
  :hash 2138907829)
 variant (extension-name string-name))

(defgmethod
 (gltflight+set-additional-data :class 'gltflight :bind "set_additional_data"
  :hash 3776071444)
 :void (extension-name string-name) (additional-data variant))