(common-lisp:in-package :%godot)


(defgproperty physics-test-motion-parameters-2d+from
 'physics-test-motion-parameters-2d :get
 'physics-test-motion-parameters-2d+get-from :set
 'physics-test-motion-parameters-2d+set-from)

(defgproperty physics-test-motion-parameters-2d+motion
 'physics-test-motion-parameters-2d :get
 'physics-test-motion-parameters-2d+get-motion :set
 'physics-test-motion-parameters-2d+set-motion)

(defgproperty physics-test-motion-parameters-2d+margin
 'physics-test-motion-parameters-2d :get
 'physics-test-motion-parameters-2d+get-margin :set
 'physics-test-motion-parameters-2d+set-margin)

(defgproperty physics-test-motion-parameters-2d+collide-separation-ray
 'physics-test-motion-parameters-2d :get
 'physics-test-motion-parameters-2d+is-collide-separation-ray-enabled :set
 'physics-test-motion-parameters-2d+set-collide-separation-ray-enabled)

(defgproperty physics-test-motion-parameters-2d+exclude-bodies
 'physics-test-motion-parameters-2d :get
 'physics-test-motion-parameters-2d+get-exclude-bodies :set
 'physics-test-motion-parameters-2d+set-exclude-bodies)

(defgproperty physics-test-motion-parameters-2d+exclude-objects
 'physics-test-motion-parameters-2d :get
 'physics-test-motion-parameters-2d+get-exclude-objects :set
 'physics-test-motion-parameters-2d+set-exclude-objects)

(defgproperty physics-test-motion-parameters-2d+recovery-as-collision
 'physics-test-motion-parameters-2d :get
 'physics-test-motion-parameters-2d+is-recovery-as-collision-enabled :set
 'physics-test-motion-parameters-2d+set-recovery-as-collision-enabled)