(common-lisp:in-package :%godot)


(defgmethod
 (gltfphysics-body+from-node :class 'gltfphysics-body :bind "from_node" :hash
  420544174 :static common-lisp:t)
 gltfphysics-body (body-node collision-object-3d))

(defgmethod
 (gltfphysics-body+to-node :class 'gltfphysics-body :bind "to_node" :hash
  3224013656)
 collision-object-3d)

(defgmethod
 (gltfphysics-body+from-dictionary :class 'gltfphysics-body :bind
  "from_dictionary" :hash 1177544336 :static common-lisp:t)
 gltfphysics-body (dictionary dictionary))

(defgmethod
 (gltfphysics-body+to-dictionary :class 'gltfphysics-body :bind "to_dictionary"
  :hash 3102165223)
 dictionary)

(defgmethod
 (gltfphysics-body+get-body-type :class 'gltfphysics-body :bind "get_body_type"
  :hash 201670096)
 string)

(defgmethod
 (gltfphysics-body+set-body-type :class 'gltfphysics-body :bind "set_body_type"
  :hash 83702148)
 :void (body-type string))

(defgmethod
 (gltfphysics-body+get-mass :class 'gltfphysics-body :bind "get_mass" :hash
  1740695150)
 float)

(defgmethod
 (gltfphysics-body+set-mass :class 'gltfphysics-body :bind "set_mass" :hash
  373806689)
 :void (mass float))

(defgmethod
 (gltfphysics-body+get-linear-velocity :class 'gltfphysics-body :bind
  "get_linear_velocity" :hash 3360562783)
 vector-3)

(defgmethod
 (gltfphysics-body+set-linear-velocity :class 'gltfphysics-body :bind
  "set_linear_velocity" :hash 3460891852)
 :void (linear-velocity vector-3))

(defgmethod
 (gltfphysics-body+get-angular-velocity :class 'gltfphysics-body :bind
  "get_angular_velocity" :hash 3360562783)
 vector-3)

(defgmethod
 (gltfphysics-body+set-angular-velocity :class 'gltfphysics-body :bind
  "set_angular_velocity" :hash 3460891852)
 :void (angular-velocity vector-3))

(defgmethod
 (gltfphysics-body+get-center-of-mass :class 'gltfphysics-body :bind
  "get_center_of_mass" :hash 3360562783)
 vector-3)

(defgmethod
 (gltfphysics-body+set-center-of-mass :class 'gltfphysics-body :bind
  "set_center_of_mass" :hash 3460891852)
 :void (center-of-mass vector-3))

(defgmethod
 (gltfphysics-body+get-inertia-diagonal :class 'gltfphysics-body :bind
  "get_inertia_diagonal" :hash 3360562783)
 vector-3)

(defgmethod
 (gltfphysics-body+set-inertia-diagonal :class 'gltfphysics-body :bind
  "set_inertia_diagonal" :hash 3460891852)
 :void (inertia-diagonal vector-3))

(defgmethod
 (gltfphysics-body+get-inertia-orientation :class 'gltfphysics-body :bind
  "get_inertia_orientation" :hash 1222331677)
 quaternion)

(defgmethod
 (gltfphysics-body+set-inertia-orientation :class 'gltfphysics-body :bind
  "set_inertia_orientation" :hash 1727505552)
 :void (inertia-orientation quaternion))

(defgmethod
 (gltfphysics-body+get-inertia-tensor :class 'gltfphysics-body :bind
  "get_inertia_tensor" :hash 2716978435)
 basis)

(defgmethod
 (gltfphysics-body+set-inertia-tensor :class 'gltfphysics-body :bind
  "set_inertia_tensor" :hash 1055510324)
 :void (inertia-tensor basis))