(common-lisp:in-package :%godot)


(defgmethod
 (open-xrrender-model+get-top-level-path :class 'open-xrrender-model :bind
  "get_top_level_path" :hash 201670096)
 string)

(defgmethod
 (open-xrrender-model+get-render-model :class 'open-xrrender-model :bind
  "get_render_model" :hash 2944877500)
 rid)

(defgmethod
 (open-xrrender-model+set-render-model :class 'open-xrrender-model :bind
  "set_render_model" :hash 2722037293)
 :void (render-model rid))