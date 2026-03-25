(common-lisp:in-package :%godot)


(defgmethod
 (render-scene-data+get-cam-transform :class 'render-scene-data :bind
  "get_cam_transform" :hash 3229777777)
 transform-3d)

(defgmethod
 (render-scene-data+get-cam-projection :class 'render-scene-data :bind
  "get_cam_projection" :hash 2910717950)
 projection)

(defgmethod
 (render-scene-data+get-view-count :class 'render-scene-data :bind
  "get_view_count" :hash 3905245786)
 int)

(defgmethod
 (render-scene-data+get-view-eye-offset :class 'render-scene-data :bind
  "get_view_eye_offset" :hash 711720468)
 vector-3 (view int))

(defgmethod
 (render-scene-data+get-view-projection :class 'render-scene-data :bind
  "get_view_projection" :hash 3179846605)
 projection (view int))

(defgmethod
 (render-scene-data+get-uniform-buffer :class 'render-scene-data :bind
  "get_uniform_buffer" :hash 2944877500)
 rid)