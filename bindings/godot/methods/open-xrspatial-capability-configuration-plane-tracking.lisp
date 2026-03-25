(common-lisp:in-package :%godot)


(defgmethod
 (open-xrspatial-capability-configuration-plane-tracking+supports-mesh-2d
  :class 'open-xrspatial-capability-configuration-plane-tracking :bind
  "supports_mesh_2d" :hash 2240911060)
 bool)

(defgmethod
 (open-xrspatial-capability-configuration-plane-tracking+supports-polygons
  :class 'open-xrspatial-capability-configuration-plane-tracking :bind
  "supports_polygons" :hash 2240911060)
 bool)

(defgmethod
 (open-xrspatial-capability-configuration-plane-tracking+supports-labels :class
  'open-xrspatial-capability-configuration-plane-tracking :bind
  "supports_labels" :hash 2240911060)
 bool)

(defgmethod
 (open-xrspatial-capability-configuration-plane-tracking+get-enabled-components
  :class 'open-xrspatial-capability-configuration-plane-tracking :bind
  "get_enabled_components" :hash 235988956)
 packed-int-64array)