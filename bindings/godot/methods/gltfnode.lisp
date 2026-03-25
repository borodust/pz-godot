(common-lisp:in-package :%godot)


(defgmethod
 (gltfnode+get-original-name :class 'gltfnode :bind "get_original_name" :hash
  2841200299)
 string)

(defgmethod
 (gltfnode+set-original-name :class 'gltfnode :bind "set_original_name" :hash
  83702148)
 :void (original-name string))

(defgmethod
 (gltfnode+get-parent :class 'gltfnode :bind "get_parent" :hash 2455072627) int)

(defgmethod
 (gltfnode+set-parent :class 'gltfnode :bind "set_parent" :hash 1286410249)
 :void (parent int))

(defgmethod
 (gltfnode+get-height :class 'gltfnode :bind "get_height" :hash 2455072627) int)

(defgmethod
 (gltfnode+set-height :class 'gltfnode :bind "set_height" :hash 1286410249)
 :void (height int))

(defgmethod
 (gltfnode+get-xform :class 'gltfnode :bind "get_xform" :hash 4183770049)
 transform-3d)

(defgmethod
 (gltfnode+set-xform :class 'gltfnode :bind "set_xform" :hash 2952846383) :void
 (xform transform-3d))

(defgmethod
 (gltfnode+get-mesh :class 'gltfnode :bind "get_mesh" :hash 2455072627) int)

(defgmethod
 (gltfnode+set-mesh :class 'gltfnode :bind "set_mesh" :hash 1286410249) :void
 (mesh int))

(defgmethod
 (gltfnode+get-camera :class 'gltfnode :bind "get_camera" :hash 2455072627) int)

(defgmethod
 (gltfnode+set-camera :class 'gltfnode :bind "set_camera" :hash 1286410249)
 :void (camera int))

(defgmethod
 (gltfnode+get-skin :class 'gltfnode :bind "get_skin" :hash 2455072627) int)

(defgmethod
 (gltfnode+set-skin :class 'gltfnode :bind "set_skin" :hash 1286410249) :void
 (skin int))

(defgmethod
 (gltfnode+get-skeleton :class 'gltfnode :bind "get_skeleton" :hash 2455072627)
 int)

(defgmethod
 (gltfnode+set-skeleton :class 'gltfnode :bind "set_skeleton" :hash 1286410249)
 :void (skeleton int))

(defgmethod
 (gltfnode+get-position :class 'gltfnode :bind "get_position" :hash 3783033775)
 vector-3)

(defgmethod
 (gltfnode+set-position :class 'gltfnode :bind "set_position" :hash 3460891852)
 :void (position vector-3))

(defgmethod
 (gltfnode+get-rotation :class 'gltfnode :bind "get_rotation" :hash 2916281908)
 quaternion)

(defgmethod
 (gltfnode+set-rotation :class 'gltfnode :bind "set_rotation" :hash 1727505552)
 :void (rotation quaternion))

(defgmethod
 (gltfnode+get-scale :class 'gltfnode :bind "get_scale" :hash 3783033775)
 vector-3)

(defgmethod
 (gltfnode+set-scale :class 'gltfnode :bind "set_scale" :hash 3460891852) :void
 (scale vector-3))

(defgmethod
 (gltfnode+get-children :class 'gltfnode :bind "get_children" :hash 969006518)
 packed-int-32array)

(defgmethod
 (gltfnode+set-children :class 'gltfnode :bind "set_children" :hash 3614634198)
 :void (children packed-int-32array))

(defgmethod
 (gltfnode+append-child-index :class 'gltfnode :bind "append_child_index" :hash
  1286410249)
 :void (child-index int))

(defgmethod
 (gltfnode+get-light :class 'gltfnode :bind "get_light" :hash 2455072627) int)

(defgmethod
 (gltfnode+set-light :class 'gltfnode :bind "set_light" :hash 1286410249) :void
 (light int))

(defgmethod
 (gltfnode+get-visible :class 'gltfnode :bind "get_visible" :hash 2240911060)
 bool)

(defgmethod
 (gltfnode+set-visible :class 'gltfnode :bind "set_visible" :hash 2586408642)
 :void (visible bool))

(defgmethod
 (gltfnode+get-additional-data :class 'gltfnode :bind "get_additional_data"
  :hash 2138907829)
 variant (extension-name string-name))

(defgmethod
 (gltfnode+set-additional-data :class 'gltfnode :bind "set_additional_data"
  :hash 3776071444)
 :void (extension-name string-name) (additional-data variant))

(defgmethod
 (gltfnode+get-scene-node-path :class 'gltfnode :bind "get_scene_node_path"
  :hash 573359477)
 node-path (gltf-state gltfstate) (handle-skeletons bool))