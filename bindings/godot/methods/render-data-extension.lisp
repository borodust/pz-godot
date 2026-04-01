(common-lisp:in-package :%godot)


(defgmethod
 (render-data-extension+%get-render-scene-buffers :class 'render-data-extension
  :bind "_get_render_scene_buffers" :hash 2793216201 :virtual common-lisp:t)
 render-scene-buffers)

(defgmethod
 (render-data-extension+%get-render-scene-data :class 'render-data-extension
  :bind "_get_render_scene_data" :hash 1288715698 :virtual common-lisp:t)
 render-scene-data)

(defgmethod
 (render-data-extension+%get-environment :class 'render-data-extension :bind
  "_get_environment" :hash 2944877500 :virtual common-lisp:t)
 rid)

(defgmethod
 (render-data-extension+%get-camera-attributes :class 'render-data-extension
  :bind "_get_camera_attributes" :hash 2944877500 :virtual common-lisp:t)
 rid)