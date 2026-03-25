(common-lisp:in-package :%godot)


(defgmethod
 (gltfphysics-shape+from-node :class 'gltfphysics-shape :bind "from_node" :hash
  3613751275 :static common-lisp:t)
 gltfphysics-shape (shape-node collision-shape-3d))

(defgmethod
 (gltfphysics-shape+to-node :class 'gltfphysics-shape :bind "to_node" :hash
  563689933)
 collision-shape-3d (cache-shapes bool))

(defgmethod
 (gltfphysics-shape+from-resource :class 'gltfphysics-shape :bind
  "from_resource" :hash 3845569786 :static common-lisp:t)
 gltfphysics-shape (shape-resource shape-3d))

(defgmethod
 (gltfphysics-shape+to-resource :class 'gltfphysics-shape :bind "to_resource"
  :hash 1913542110)
 shape-3d (cache-shapes bool))

(defgmethod
 (gltfphysics-shape+from-dictionary :class 'gltfphysics-shape :bind
  "from_dictionary" :hash 2390691823 :static common-lisp:t)
 gltfphysics-shape (dictionary dictionary))

(defgmethod
 (gltfphysics-shape+to-dictionary :class 'gltfphysics-shape :bind
  "to_dictionary" :hash 3102165223)
 dictionary)

(defgmethod
 (gltfphysics-shape+get-shape-type :class 'gltfphysics-shape :bind
  "get_shape_type" :hash 201670096)
 string)

(defgmethod
 (gltfphysics-shape+set-shape-type :class 'gltfphysics-shape :bind
  "set_shape_type" :hash 83702148)
 :void (shape-type string))

(defgmethod
 (gltfphysics-shape+get-size :class 'gltfphysics-shape :bind "get_size" :hash
  3360562783)
 vector-3)

(defgmethod
 (gltfphysics-shape+set-size :class 'gltfphysics-shape :bind "set_size" :hash
  3460891852)
 :void (size vector-3))

(defgmethod
 (gltfphysics-shape+get-radius :class 'gltfphysics-shape :bind "get_radius"
  :hash 1740695150)
 float)

(defgmethod
 (gltfphysics-shape+set-radius :class 'gltfphysics-shape :bind "set_radius"
  :hash 373806689)
 :void (radius float))

(defgmethod
 (gltfphysics-shape+get-height :class 'gltfphysics-shape :bind "get_height"
  :hash 1740695150)
 float)

(defgmethod
 (gltfphysics-shape+set-height :class 'gltfphysics-shape :bind "set_height"
  :hash 373806689)
 :void (height float))

(defgmethod
 (gltfphysics-shape+get-is-trigger :class 'gltfphysics-shape :bind
  "get_is_trigger" :hash 36873697)
 bool)

(defgmethod
 (gltfphysics-shape+set-is-trigger :class 'gltfphysics-shape :bind
  "set_is_trigger" :hash 2586408642)
 :void (is-trigger bool))

(defgmethod
 (gltfphysics-shape+get-mesh-index :class 'gltfphysics-shape :bind
  "get_mesh_index" :hash 3905245786)
 int)

(defgmethod
 (gltfphysics-shape+set-mesh-index :class 'gltfphysics-shape :bind
  "set_mesh_index" :hash 1286410249)
 :void (mesh-index int))

(defgmethod
 (gltfphysics-shape+get-importer-mesh :class 'gltfphysics-shape :bind
  "get_importer_mesh" :hash 3161779525)
 importer-mesh)

(defgmethod
 (gltfphysics-shape+set-importer-mesh :class 'gltfphysics-shape :bind
  "set_importer_mesh" :hash 2255166972)
 :void (importer-mesh importer-mesh))