(common-lisp:in-package :%godot)


(defgproperty gltfobject-model-property+gltf-to-godot-expression
 'gltfobject-model-property :get
 'gltfobject-model-property+get-gltf-to-godot-expression :set
 'gltfobject-model-property+set-gltf-to-godot-expression)

(defgproperty gltfobject-model-property+godot-to-gltf-expression
 'gltfobject-model-property :get
 'gltfobject-model-property+get-godot-to-gltf-expression :set
 'gltfobject-model-property+set-godot-to-gltf-expression)

(defgproperty gltfobject-model-property+node-paths 'gltfobject-model-property
 :get 'gltfobject-model-property+get-node-paths :set
 'gltfobject-model-property+set-node-paths)

(defgproperty gltfobject-model-property+object-model-type
 'gltfobject-model-property :get
 'gltfobject-model-property+get-object-model-type :set
 'gltfobject-model-property+set-object-model-type)

(defgproperty gltfobject-model-property+json-pointers
 'gltfobject-model-property :get 'gltfobject-model-property+get-json-pointers
 :set 'gltfobject-model-property+set-json-pointers)

(defgproperty gltfobject-model-property+variant-type 'gltfobject-model-property
 :get 'gltfobject-model-property+get-variant-type :set
 'gltfobject-model-property+set-variant-type)