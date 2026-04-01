(common-lisp:in-package :%godot)


(defgmethod
 (render-scene-data-extension+%get-cam-transform :class
  'render-scene-data-extension :bind "_get_cam_transform" :hash 3229777777
  :virtual common-lisp:t)
 transform-3d)

(defgmethod
 (render-scene-data-extension+%get-cam-projection :class
  'render-scene-data-extension :bind "_get_cam_projection" :hash 2910717950
  :virtual common-lisp:t)
 projection)

(defgmethod
 (render-scene-data-extension+%get-view-count :class
  'render-scene-data-extension :bind "_get_view_count" :hash 3905245786
  :virtual common-lisp:t)
 int)

(defgmethod
 (render-scene-data-extension+%get-view-eye-offset :class
  'render-scene-data-extension :bind "_get_view_eye_offset" :hash 711720468
  :virtual common-lisp:t)
 vector-3 (view int))

(defgmethod
 (render-scene-data-extension+%get-view-projection :class
  'render-scene-data-extension :bind "_get_view_projection" :hash 3179846605
  :virtual common-lisp:t)
 projection (view int))

(defgmethod
 (render-scene-data-extension+%get-uniform-buffer :class
  'render-scene-data-extension :bind "_get_uniform_buffer" :hash 2944877500
  :virtual common-lisp:t)
 rid)