(common-lisp:in-package :%godot)


(defgmethod
 (gltfmesh+get-original-name :class 'gltfmesh :bind "get_original_name" :hash
  2841200299)
 string)

(defgmethod
 (gltfmesh+set-original-name :class 'gltfmesh :bind "set_original_name" :hash
  83702148)
 :void (original-name string))

(defgmethod
 (gltfmesh+get-mesh :class 'gltfmesh :bind "get_mesh" :hash 3754628756)
 importer-mesh)

(defgmethod
 (gltfmesh+set-mesh :class 'gltfmesh :bind "set_mesh" :hash 2255166972) :void
 (mesh importer-mesh))

(defgmethod
 (gltfmesh+get-blend-weights :class 'gltfmesh :bind "get_blend_weights" :hash
  2445143706)
 packed-float-32array)

(defgmethod
 (gltfmesh+set-blend-weights :class 'gltfmesh :bind "set_blend_weights" :hash
  2899603908)
 :void (blend-weights packed-float-32array))

(defgmethod
 (gltfmesh+get-instance-materials :class 'gltfmesh :bind
  "get_instance_materials" :hash 2915620761)
 array)

(defgmethod
 (gltfmesh+set-instance-materials :class 'gltfmesh :bind
  "set_instance_materials" :hash 381264803)
 :void (instance-materials array))

(defgmethod
 (gltfmesh+get-additional-data :class 'gltfmesh :bind "get_additional_data"
  :hash 2138907829)
 variant (extension-name string-name))

(defgmethod
 (gltfmesh+set-additional-data :class 'gltfmesh :bind "set_additional_data"
  :hash 3776071444)
 :void (extension-name string-name) (additional-data variant))