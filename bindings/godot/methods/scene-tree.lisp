(common-lisp:in-package :%godot)


(defgmethod
 (scene-tree+get-root :class 'scene-tree :bind "get_root" :hash 1757182445)
 window)

(defgmethod
 (scene-tree+has-group :class 'scene-tree :bind "has_group" :hash 2619796661)
 bool (name string-name))

(defgmethod
 (scene-tree+is-accessibility-enabled :class 'scene-tree :bind
  "is_accessibility_enabled" :hash 36873697)
 bool)

(defgmethod
 (scene-tree+is-accessibility-supported :class 'scene-tree :bind
  "is_accessibility_supported" :hash 36873697)
 bool)

(defgmethod
 (scene-tree+is-auto-accept-quit :class 'scene-tree :bind "is_auto_accept_quit"
  :hash 36873697)
 bool)

(defgmethod
 (scene-tree+set-auto-accept-quit :class 'scene-tree :bind
  "set_auto_accept_quit" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (scene-tree+is-quit-on-go-back :class 'scene-tree :bind "is_quit_on_go_back"
  :hash 36873697)
 bool)

(defgmethod
 (scene-tree+set-quit-on-go-back :class 'scene-tree :bind "set_quit_on_go_back"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (scene-tree+set-debug-collisions-hint :class 'scene-tree :bind
  "set_debug_collisions_hint" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (scene-tree+is-debugging-collisions-hint :class 'scene-tree :bind
  "is_debugging_collisions_hint" :hash 36873697)
 bool)

(defgmethod
 (scene-tree+set-debug-paths-hint :class 'scene-tree :bind
  "set_debug_paths_hint" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (scene-tree+is-debugging-paths-hint :class 'scene-tree :bind
  "is_debugging_paths_hint" :hash 36873697)
 bool)

(defgmethod
 (scene-tree+set-debug-navigation-hint :class 'scene-tree :bind
  "set_debug_navigation_hint" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (scene-tree+is-debugging-navigation-hint :class 'scene-tree :bind
  "is_debugging_navigation_hint" :hash 36873697)
 bool)

(defgmethod
 (scene-tree+set-edited-scene-root :class 'scene-tree :bind
  "set_edited_scene_root" :hash 1078189570)
 :void (scene node))

(defgmethod
 (scene-tree+get-edited-scene-root :class 'scene-tree :bind
  "get_edited_scene_root" :hash 3160264692)
 node)

(defgmethod
 (scene-tree+set-pause :class 'scene-tree :bind "set_pause" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (scene-tree+is-paused :class 'scene-tree :bind "is_paused" :hash 36873697)
 bool)

(defgmethod
 (scene-tree+create-timer :class 'scene-tree :bind "create_timer" :hash
  2709170273)
 scene-tree-timer (time-sec float) (process-always bool)
 (process-in-physics bool) (ignore-time-scale bool))

(defgmethod
 (scene-tree+create-tween :class 'scene-tree :bind "create_tween" :hash
  3426978995)
 tween)

(defgmethod
 (scene-tree+get-processed-tweens :class 'scene-tree :bind
  "get_processed_tweens" :hash 2915620761)
 array)

(defgmethod
 (scene-tree+get-node-count :class 'scene-tree :bind "get_node_count" :hash
  3905245786)
 int)

(defgmethod
 (scene-tree+get-frame :class 'scene-tree :bind "get_frame" :hash 3905245786)
 int)

(defgmethod (scene-tree+quit :class 'scene-tree :bind "quit" :hash 1995695955)
 :void (exit-code int))

(defgmethod
 (scene-tree+set-physics-interpolation-enabled :class 'scene-tree :bind
  "set_physics_interpolation_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (scene-tree+is-physics-interpolation-enabled :class 'scene-tree :bind
  "is_physics_interpolation_enabled" :hash 36873697)
 bool)

(defgmethod
 (scene-tree+queue-delete :class 'scene-tree :bind "queue_delete" :hash
  3975164845)
 :void (obj object))

(defgmethod
 (scene-tree+call-group-flags :class 'scene-tree :bind "call_group_flags" :hash
  1527739229 :vararg common-lisp:t)
 :void (flags int) (group string-name) (method string-name))

(defgmethod
 (scene-tree+notify-group-flags :class 'scene-tree :bind "notify_group_flags"
  :hash 1245489420)
 :void (call-flags int) (group string-name) (notification int))

(defgmethod
 (scene-tree+set-group-flags :class 'scene-tree :bind "set_group_flags" :hash
  3497599527)
 :void (call-flags int) (group string-name) (property string) (value variant))

(defgmethod
 (scene-tree+call-group :class 'scene-tree :bind "call_group" :hash 1257962832
  :vararg common-lisp:t)
 :void (group string-name) (method string-name))

(defgmethod
 (scene-tree+notify-group :class 'scene-tree :bind "notify_group" :hash
  2415702435)
 :void (group string-name) (notification int))

(defgmethod
 (scene-tree+set-group :class 'scene-tree :bind "set_group" :hash 1279312029)
 :void (group string-name) (property string) (value variant))

(defgmethod
 (scene-tree+get-nodes-in-group :class 'scene-tree :bind "get_nodes_in_group"
  :hash 689397652)
 array (group string-name))

(defgmethod
 (scene-tree+get-first-node-in-group :class 'scene-tree :bind
  "get_first_node_in_group" :hash 4071044623)
 node (group string-name))

(defgmethod
 (scene-tree+get-node-count-in-group :class 'scene-tree :bind
  "get_node_count_in_group" :hash 2458036349)
 int (group string-name))

(defgmethod
 (scene-tree+set-current-scene :class 'scene-tree :bind "set_current_scene"
  :hash 1078189570)
 :void (child-node node))

(defgmethod
 (scene-tree+get-current-scene :class 'scene-tree :bind "get_current_scene"
  :hash 3160264692)
 node)

(defgmethod
 (scene-tree+change-scene-to-file :class 'scene-tree :bind
  "change_scene_to_file" :hash 166001499)
 error (path string))

(defgmethod
 (scene-tree+change-scene-to-packed :class 'scene-tree :bind
  "change_scene_to_packed" :hash 107349098)
 error (packed-scene packed-scene))

(defgmethod
 (scene-tree+change-scene-to-node :class 'scene-tree :bind
  "change_scene_to_node" :hash 2584678054)
 error (node node))

(defgmethod
 (scene-tree+reload-current-scene :class 'scene-tree :bind
  "reload_current_scene" :hash 166280745)
 error)

(defgmethod
 (scene-tree+unload-current-scene :class 'scene-tree :bind
  "unload_current_scene" :hash 3218959716)
 :void)

(defgmethod
 (scene-tree+set-multiplayer :class 'scene-tree :bind "set_multiplayer" :hash
  2385607013)
 :void (multiplayer multiplayer-api) (root-path node-path))

(defgmethod
 (scene-tree+get-multiplayer :class 'scene-tree :bind "get_multiplayer" :hash
  3453401404)
 multiplayer-api (for-path node-path))

(defgmethod
 (scene-tree+set-multiplayer-poll-enabled :class 'scene-tree :bind
  "set_multiplayer_poll_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (scene-tree+is-multiplayer-poll-enabled :class 'scene-tree :bind
  "is_multiplayer_poll_enabled" :hash 36873697)
 bool)