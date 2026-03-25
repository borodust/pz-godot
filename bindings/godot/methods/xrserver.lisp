(common-lisp:in-package :%godot)


(defgmethod
 (xrserver+get-world-scale :class 'xrserver :bind "get_world_scale" :hash
  1740695150)
 float)

(defgmethod
 (xrserver+set-world-scale :class 'xrserver :bind "set_world_scale" :hash
  373806689)
 :void (scale float))

(defgmethod
 (xrserver+get-world-origin :class 'xrserver :bind "get_world_origin" :hash
  3229777777)
 transform-3d)

(defgmethod
 (xrserver+set-world-origin :class 'xrserver :bind "set_world_origin" :hash
  2952846383)
 :void (world-origin transform-3d))

(defgmethod
 (xrserver+get-reference-frame :class 'xrserver :bind "get_reference_frame"
  :hash 3229777777)
 transform-3d)

(defgmethod
 (xrserver+clear-reference-frame :class 'xrserver :bind "clear_reference_frame"
  :hash 3218959716)
 :void)

(defgmethod
 (xrserver+center-on-hmd :class 'xrserver :bind "center_on_hmd" :hash
  1450904707)
 :void (rotation-mode xrserver+rotation-mode) (keep-height bool))

(defgmethod
 (xrserver+get-hmd-transform :class 'xrserver :bind "get_hmd_transform" :hash
  4183770049)
 transform-3d)

(defgmethod
 (xrserver+set-camera-locked-to-origin :class 'xrserver :bind
  "set_camera_locked_to_origin" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (xrserver+is-camera-locked-to-origin :class 'xrserver :bind
  "is_camera_locked_to_origin" :hash 36873697)
 bool)

(defgmethod
 (xrserver+add-interface :class 'xrserver :bind "add_interface" :hash
  1898711491)
 :void (interface xrinterface))

(defgmethod
 (xrserver+get-interface-count :class 'xrserver :bind "get_interface_count"
  :hash 3905245786)
 int)

(defgmethod
 (xrserver+remove-interface :class 'xrserver :bind "remove_interface" :hash
  1898711491)
 :void (interface xrinterface))

(defgmethod
 (xrserver+get-interface :class 'xrserver :bind "get_interface" :hash
  4237347919)
 xrinterface (idx int))

(defgmethod
 (xrserver+get-interfaces :class 'xrserver :bind "get_interfaces" :hash
  3995934104)
 array)

(defgmethod
 (xrserver+find-interface :class 'xrserver :bind "find_interface" :hash
  1395192955)
 xrinterface (name string))

(defgmethod
 (xrserver+add-tracker :class 'xrserver :bind "add_tracker" :hash 684804553)
 :void (tracker xrtracker))

(defgmethod
 (xrserver+remove-tracker :class 'xrserver :bind "remove_tracker" :hash
  684804553)
 :void (tracker xrtracker))

(defgmethod
 (xrserver+get-trackers :class 'xrserver :bind "get_trackers" :hash 3554694381)
 dictionary (tracker-types int))

(defgmethod
 (xrserver+get-tracker :class 'xrserver :bind "get_tracker" :hash 147382240)
 xrtracker (tracker-name string-name))

(defgmethod
 (xrserver+get-primary-interface :class 'xrserver :bind "get_primary_interface"
  :hash 2143545064)
 xrinterface)

(defgmethod
 (xrserver+set-primary-interface :class 'xrserver :bind "set_primary_interface"
  :hash 1898711491)
 :void (interface xrinterface))