(common-lisp:in-package :%godot)


(defgproperty gltfmesh+original-name 'gltfmesh :get 'gltfmesh+get-original-name
 :set 'gltfmesh+set-original-name)

(defgproperty gltfmesh+mesh 'gltfmesh :get 'gltfmesh+get-mesh :set
 'gltfmesh+set-mesh)

(defgproperty gltfmesh+blend-weights 'gltfmesh :get 'gltfmesh+get-blend-weights
 :set 'gltfmesh+set-blend-weights)

(defgproperty gltfmesh+instance-materials 'gltfmesh :get
 'gltfmesh+get-instance-materials :set 'gltfmesh+set-instance-materials)