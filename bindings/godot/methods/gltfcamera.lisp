(common-lisp:in-package :%godot)


(defgmethod
 (gltfcamera+from-node :class 'gltfcamera :bind "from_node" :hash 237784
  :static common-lisp:t)
 gltfcamera (camera-node camera-3d))

(defgmethod
 (gltfcamera+to-node :class 'gltfcamera :bind "to_node" :hash 2285090890)
 camera-3d)

(defgmethod
 (gltfcamera+from-dictionary :class 'gltfcamera :bind "from_dictionary" :hash
  2495512509 :static common-lisp:t)
 gltfcamera (dictionary dictionary))

(defgmethod
 (gltfcamera+to-dictionary :class 'gltfcamera :bind "to_dictionary" :hash
  3102165223)
 dictionary)

(defgmethod
 (gltfcamera+get-perspective :class 'gltfcamera :bind "get_perspective" :hash
  36873697)
 bool)

(defgmethod
 (gltfcamera+set-perspective :class 'gltfcamera :bind "set_perspective" :hash
  2586408642)
 :void (perspective bool))

(defgmethod
 (gltfcamera+get-fov :class 'gltfcamera :bind "get_fov" :hash 1740695150) float)

(defgmethod
 (gltfcamera+set-fov :class 'gltfcamera :bind "set_fov" :hash 373806689) :void
 (fov float))

(defgmethod
 (gltfcamera+get-size-mag :class 'gltfcamera :bind "get_size_mag" :hash
  1740695150)
 float)

(defgmethod
 (gltfcamera+set-size-mag :class 'gltfcamera :bind "set_size_mag" :hash
  373806689)
 :void (size-mag float))

(defgmethod
 (gltfcamera+get-depth-far :class 'gltfcamera :bind "get_depth_far" :hash
  1740695150)
 float)

(defgmethod
 (gltfcamera+set-depth-far :class 'gltfcamera :bind "set_depth_far" :hash
  373806689)
 :void (zdepth-far float))

(defgmethod
 (gltfcamera+get-depth-near :class 'gltfcamera :bind "get_depth_near" :hash
  1740695150)
 float)

(defgmethod
 (gltfcamera+set-depth-near :class 'gltfcamera :bind "set_depth_near" :hash
  373806689)
 :void (zdepth-near float))