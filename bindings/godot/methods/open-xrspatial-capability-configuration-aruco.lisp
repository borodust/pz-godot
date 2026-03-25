(common-lisp:in-package :%godot)


(defgmethod
 (open-xrspatial-capability-configuration-aruco+get-enabled-components :class
  'open-xrspatial-capability-configuration-aruco :bind "get_enabled_components"
  :hash 235988956)
 packed-int-64array)

(defgmethod
 (open-xrspatial-capability-configuration-aruco+set-aruco-dict :class
  'open-xrspatial-capability-configuration-aruco :bind "set_aruco_dict" :hash
  2268055963)
 :void (aruco-dict open-xrspatial-capability-configuration-aruco+aruco-dict))

(defgmethod
 (open-xrspatial-capability-configuration-aruco+get-aruco-dict :class
  'open-xrspatial-capability-configuration-aruco :bind "get_aruco_dict" :hash
  1080386209)
 open-xrspatial-capability-configuration-aruco+aruco-dict)