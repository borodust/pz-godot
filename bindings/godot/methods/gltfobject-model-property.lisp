(common-lisp:in-package :%godot)


(defgmethod
 (gltfobject-model-property+append-node-path :class 'gltfobject-model-property
  :bind "append_node_path" :hash 1348162250)
 :void (node-path node-path))

(defgmethod
 (gltfobject-model-property+append-path-to-property :class
  'gltfobject-model-property :bind "append_path_to_property" :hash 1331931644)
 :void (node-path node-path) (prop-name string-name))

(defgmethod
 (gltfobject-model-property+get-accessor-type :class 'gltfobject-model-property
  :bind "get_accessor_type" :hash 1998183368)
 gltfaccessor+gltfaccessor-type)

(defgmethod
 (gltfobject-model-property+get-gltf-to-godot-expression :class
  'gltfobject-model-property :bind "get_gltf_to_godot_expression" :hash
  2240072449)
 expression)

(defgmethod
 (gltfobject-model-property+set-gltf-to-godot-expression :class
  'gltfobject-model-property :bind "set_gltf_to_godot_expression" :hash
  1815845073)
 :void (gltf-to-godot-expr expression))

(defgmethod
 (gltfobject-model-property+get-godot-to-gltf-expression :class
  'gltfobject-model-property :bind "get_godot_to_gltf_expression" :hash
  2240072449)
 expression)

(defgmethod
 (gltfobject-model-property+set-godot-to-gltf-expression :class
  'gltfobject-model-property :bind "set_godot_to_gltf_expression" :hash
  1815845073)
 :void (godot-to-gltf-expr expression))

(defgmethod
 (gltfobject-model-property+get-node-paths :class 'gltfobject-model-property
  :bind "get_node_paths" :hash 3995934104)
 array)

(defgmethod
 (gltfobject-model-property+has-node-paths :class 'gltfobject-model-property
  :bind "has_node_paths" :hash 36873697)
 bool)

(defgmethod
 (gltfobject-model-property+set-node-paths :class 'gltfobject-model-property
  :bind "set_node_paths" :hash 381264803)
 :void (node-paths array))

(defgmethod
 (gltfobject-model-property+get-object-model-type :class
  'gltfobject-model-property :bind "get_object_model_type" :hash 1094778507)
 gltfobject-model-property+gltfobject-model-type)

(defgmethod
 (gltfobject-model-property+set-object-model-type :class
  'gltfobject-model-property :bind "set_object_model_type" :hash 4108684086)
 :void (type gltfobject-model-property+gltfobject-model-type))

(defgmethod
 (gltfobject-model-property+get-json-pointers :class 'gltfobject-model-property
  :bind "get_json_pointers" :hash 3995934104)
 array)

(defgmethod
 (gltfobject-model-property+has-json-pointers :class 'gltfobject-model-property
  :bind "has_json_pointers" :hash 36873697)
 bool)

(defgmethod
 (gltfobject-model-property+set-json-pointers :class 'gltfobject-model-property
  :bind "set_json_pointers" :hash 381264803)
 :void (json-pointers array))

(defgmethod
 (gltfobject-model-property+get-variant-type :class 'gltfobject-model-property
  :bind "get_variant_type" :hash 3416842102)
 variant+type)

(defgmethod
 (gltfobject-model-property+set-variant-type :class 'gltfobject-model-property
  :bind "set_variant_type" :hash 2887708385)
 :void (variant-type variant+type))

(defgmethod
 (gltfobject-model-property+set-types :class 'gltfobject-model-property :bind
  "set_types" :hash 4150728237)
 :void (variant-type variant+type)
 (obj-model-type gltfobject-model-property+gltfobject-model-type))