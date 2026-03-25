(common-lisp:in-package :%godot)


(defgmethod
 (height-map-shape-3d+set-map-width :class 'height-map-shape-3d :bind
  "set_map_width" :hash 1286410249)
 :void (width int))

(defgmethod
 (height-map-shape-3d+get-map-width :class 'height-map-shape-3d :bind
  "get_map_width" :hash 3905245786)
 int)

(defgmethod
 (height-map-shape-3d+set-map-depth :class 'height-map-shape-3d :bind
  "set_map_depth" :hash 1286410249)
 :void (height int))

(defgmethod
 (height-map-shape-3d+get-map-depth :class 'height-map-shape-3d :bind
  "get_map_depth" :hash 3905245786)
 int)

(defgmethod
 (height-map-shape-3d+set-map-data :class 'height-map-shape-3d :bind
  "set_map_data" :hash 2899603908)
 :void (data packed-float-32array))

(defgmethod
 (height-map-shape-3d+get-map-data :class 'height-map-shape-3d :bind
  "get_map_data" :hash 675695659)
 packed-float-32array)

(defgmethod
 (height-map-shape-3d+get-min-height :class 'height-map-shape-3d :bind
  "get_min_height" :hash 1740695150)
 float)

(defgmethod
 (height-map-shape-3d+get-max-height :class 'height-map-shape-3d :bind
  "get_max_height" :hash 1740695150)
 float)

(defgmethod
 (height-map-shape-3d+update-map-data-from-image :class 'height-map-shape-3d
  :bind "update_map_data_from_image" :hash 2636652979)
 :void (image image) (height-min float) (height-max float))