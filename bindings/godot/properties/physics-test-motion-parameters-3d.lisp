(common-lisp:in-package :%godot)


(defgproperty physics-test-motion-parameters-3d+from
 'physics-test-motion-parameters-3d :get
 'physics-test-motion-parameters-3d+get-from :set
 'physics-test-motion-parameters-3d+set-from)

(defgproperty physics-test-motion-parameters-3d+motion
 'physics-test-motion-parameters-3d :get
 'physics-test-motion-parameters-3d+get-motion :set
 'physics-test-motion-parameters-3d+set-motion)

(defgproperty physics-test-motion-parameters-3d+margin
 'physics-test-motion-parameters-3d :get
 'physics-test-motion-parameters-3d+get-margin :set
 'physics-test-motion-parameters-3d+set-margin)

(defgproperty physics-test-motion-parameters-3d+max-collisions
 'physics-test-motion-parameters-3d :get
 'physics-test-motion-parameters-3d+get-max-collisions :set
 'physics-test-motion-parameters-3d+set-max-collisions)

(defgproperty physics-test-motion-parameters-3d+collide-separation-ray
 'physics-test-motion-parameters-3d :get
 'physics-test-motion-parameters-3d+is-collide-separation-ray-enabled :set
 'physics-test-motion-parameters-3d+set-collide-separation-ray-enabled)

(defgproperty physics-test-motion-parameters-3d+exclude-bodies
 'physics-test-motion-parameters-3d :get
 'physics-test-motion-parameters-3d+get-exclude-bodies :set
 'physics-test-motion-parameters-3d+set-exclude-bodies)

(defgproperty physics-test-motion-parameters-3d+exclude-objects
 'physics-test-motion-parameters-3d :get
 'physics-test-motion-parameters-3d+get-exclude-objects :set
 'physics-test-motion-parameters-3d+set-exclude-objects)

(defgproperty physics-test-motion-parameters-3d+recovery-as-collision
 'physics-test-motion-parameters-3d :get
 'physics-test-motion-parameters-3d+is-recovery-as-collision-enabled :set
 'physics-test-motion-parameters-3d+set-recovery-as-collision-enabled)