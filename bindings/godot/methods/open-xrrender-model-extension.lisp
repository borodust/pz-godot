(common-lisp:in-package :%godot)


(defgmethod
 (open-xrrender-model-extension+is-active :class 'open-xrrender-model-extension
  :bind "is_active" :hash 36873697)
 bool)

(defgmethod
 (open-xrrender-model-extension+render-model-create :class
  'open-xrrender-model-extension :bind "render_model_create" :hash 937000113)
 rid (render-model-id int))

(defgmethod
 (open-xrrender-model-extension+render-model-destroy :class
  'open-xrrender-model-extension :bind "render_model_destroy" :hash 2722037293)
 :void (render-model rid))

(defgmethod
 (open-xrrender-model-extension+render-model-get-all :class
  'open-xrrender-model-extension :bind "render_model_get_all" :hash 2915620761)
 array)

(defgmethod
 (open-xrrender-model-extension+render-model-new-scene-instance :class
  'open-xrrender-model-extension :bind "render_model_new_scene_instance" :hash
  788010739)
 node-3d (render-model rid))

(defgmethod
 (open-xrrender-model-extension+render-model-get-subaction-paths :class
  'open-xrrender-model-extension :bind "render_model_get_subaction_paths" :hash
  2801473409)
 packed-string-array (render-model rid))

(defgmethod
 (open-xrrender-model-extension+render-model-get-top-level-path :class
  'open-xrrender-model-extension :bind "render_model_get_top_level_path" :hash
  642473191)
 string (render-model rid))

(defgmethod
 (open-xrrender-model-extension+render-model-get-confidence :class
  'open-xrrender-model-extension :bind "render_model_get_confidence" :hash
  2350330949)
 xrpose+tracking-confidence (render-model rid))

(defgmethod
 (open-xrrender-model-extension+render-model-get-root-transform :class
  'open-xrrender-model-extension :bind "render_model_get_root_transform" :hash
  1128465797)
 transform-3d (render-model rid))

(defgmethod
 (open-xrrender-model-extension+render-model-get-animatable-node-count :class
  'open-xrrender-model-extension :bind "render_model_get_animatable_node_count"
  :hash 2198884583)
 int (render-model rid))

(defgmethod
 (open-xrrender-model-extension+render-model-get-animatable-node-name :class
  'open-xrrender-model-extension :bind "render_model_get_animatable_node_name"
  :hash 1464764419)
 string (render-model rid) (index int))

(defgmethod
 (open-xrrender-model-extension+render-model-is-animatable-node-visible :class
  'open-xrrender-model-extension :bind
  "render_model_is_animatable_node_visible" :hash 3120086654)
 bool (render-model rid) (index int))

(defgmethod
 (open-xrrender-model-extension+render-model-get-animatable-node-transform
  :class 'open-xrrender-model-extension :bind
  "render_model_get_animatable_node_transform" :hash 1050775521)
 transform-3d (render-model rid) (index int))