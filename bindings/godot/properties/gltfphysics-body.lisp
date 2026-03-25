(common-lisp:in-package :%godot)


(defgproperty gltfphysics-body+body-type 'gltfphysics-body :get
 'gltfphysics-body+get-body-type :set 'gltfphysics-body+set-body-type)

(defgproperty gltfphysics-body+mass 'gltfphysics-body :get
 'gltfphysics-body+get-mass :set 'gltfphysics-body+set-mass)

(defgproperty gltfphysics-body+linear-velocity 'gltfphysics-body :get
 'gltfphysics-body+get-linear-velocity :set
 'gltfphysics-body+set-linear-velocity)

(defgproperty gltfphysics-body+angular-velocity 'gltfphysics-body :get
 'gltfphysics-body+get-angular-velocity :set
 'gltfphysics-body+set-angular-velocity)

(defgproperty gltfphysics-body+center-of-mass 'gltfphysics-body :get
 'gltfphysics-body+get-center-of-mass :set 'gltfphysics-body+set-center-of-mass)

(defgproperty gltfphysics-body+inertia-diagonal 'gltfphysics-body :get
 'gltfphysics-body+get-inertia-diagonal :set
 'gltfphysics-body+set-inertia-diagonal)

(defgproperty gltfphysics-body+inertia-orientation 'gltfphysics-body :get
 'gltfphysics-body+get-inertia-orientation :set
 'gltfphysics-body+set-inertia-orientation)

(defgproperty gltfphysics-body+inertia-tensor 'gltfphysics-body :get
 'gltfphysics-body+get-inertia-tensor :set 'gltfphysics-body+set-inertia-tensor)