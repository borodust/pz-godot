(common-lisp:in-package :%godot)


(defgmethod
 (resource+%setup-local-to-scene :class 'resource :bind "_setup_local_to_scene"
  :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (resource+%get-rid :class 'resource :bind "_get_rid" :hash 2944877500 :virtual
  common-lisp:t)
 rid)

(defgmethod
 (resource+%reset-state :class 'resource :bind "_reset_state" :hash 3218959716
  :virtual common-lisp:t)
 :void)

(defgmethod
 (resource+%set-path-cache :class 'resource :bind "_set_path_cache" :hash
  3089850668 :virtual common-lisp:t)
 :void (path string))

(defgmethod
 (resource+set-path :class 'resource :bind "set_path" :hash 83702148) :void
 (path string))

(defgmethod
 (resource+take-over-path :class 'resource :bind "take_over_path" :hash
  83702148)
 :void (path string))

(defgmethod
 (resource+get-path :class 'resource :bind "get_path" :hash 201670096) string)

(defgmethod
 (resource+set-path-cache :class 'resource :bind "set_path_cache" :hash
  83702148)
 :void (path string))

(defgmethod
 (resource+set-name :class 'resource :bind "set_name" :hash 83702148) :void
 (name string))

(defgmethod
 (resource+get-name :class 'resource :bind "get_name" :hash 201670096) string)

(defgmethod
 (resource+get-rid :class 'resource :bind "get_rid" :hash 2944877500) rid)

(defgmethod
 (resource+set-local-to-scene :class 'resource :bind "set_local_to_scene" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (resource+is-local-to-scene :class 'resource :bind "is_local_to_scene" :hash
  36873697)
 bool)

(defgmethod
 (resource+get-local-scene :class 'resource :bind "get_local_scene" :hash
  3160264692)
 node)

(defgmethod
 (resource+setup-local-to-scene :class 'resource :bind "setup_local_to_scene"
  :hash 3218959716)
 :void)

(defgmethod
 (resource+reset-state :class 'resource :bind "reset_state" :hash 3218959716)
 :void)

(defgmethod
 (resource+set-id-for-path :class 'resource :bind "set_id_for_path" :hash
  3186203200)
 :void (path string) (id string))

(defgmethod
 (resource+get-id-for-path :class 'resource :bind "get_id_for_path" :hash
  3135753539)
 string (path string))

(defgmethod
 (resource+is-built-in :class 'resource :bind "is_built_in" :hash 36873697)
 bool)

(defgmethod
 (resource+generate-scene-unique-id :class 'resource :bind
  "generate_scene_unique_id" :hash 2841200299 :static common-lisp:t)
 string)

(defgmethod
 (resource+set-scene-unique-id :class 'resource :bind "set_scene_unique_id"
  :hash 83702148)
 :void (id string))

(defgmethod
 (resource+get-scene-unique-id :class 'resource :bind "get_scene_unique_id"
  :hash 201670096)
 string)

(defgmethod
 (resource+emit-changed :class 'resource :bind "emit_changed" :hash 3218959716)
 :void)

(defgmethod
 (resource+duplicate :class 'resource :bind "duplicate" :hash 482882304)
 resource (deep bool))

(defgmethod
 (resource+duplicate-deep :class 'resource :bind "duplicate_deep" :hash
  905779109)
 resource (deep-subresources-mode resource+deep-duplicate-mode))