(common-lisp:in-package :%godot)


(defgmethod
 (render-data+get-render-scene-buffers :class 'render-data :bind
  "get_render_scene_buffers" :hash 2793216201)
 render-scene-buffers)

(defgmethod
 (render-data+get-render-scene-data :class 'render-data :bind
  "get_render_scene_data" :hash 1288715698)
 render-scene-data)

(defgmethod
 (render-data+get-environment :class 'render-data :bind "get_environment" :hash
  2944877500)
 rid)

(defgmethod
 (render-data+get-camera-attributes :class 'render-data :bind
  "get_camera_attributes" :hash 2944877500)
 rid)