(common-lisp:in-package :%godot)


(defgmethod
 (node+-process :class 'node :bind "_process" :hash 373806689 :virtual
  common-lisp:t)
 :void (delta float))

(defgmethod
 (node+-physics-process :class 'node :bind "_physics_process" :hash 373806689
  :virtual common-lisp:t)
 :void (delta float))

(defgmethod
 (node+-enter-tree :class 'node :bind "_enter_tree" :hash 3218959716 :virtual
  common-lisp:t)
 :void)

(defgmethod
 (node+-exit-tree :class 'node :bind "_exit_tree" :hash 3218959716 :virtual
  common-lisp:t)
 :void)

(defgmethod
 (node+-ready :class 'node :bind "_ready" :hash 3218959716 :virtual
  common-lisp:t)
 :void)

(defgmethod
 (node+-get-configuration-warnings :class 'node :bind
  "_get_configuration_warnings" :hash 1139954409 :virtual common-lisp:t)
 packed-string-array)

(defgmethod
 (node+-get-accessibility-configuration-warnings :class 'node :bind
  "_get_accessibility_configuration_warnings" :hash 1139954409 :virtual
  common-lisp:t)
 packed-string-array)

(defgmethod
 (node+-input :class 'node :bind "_input" :hash 3754044979 :virtual
  common-lisp:t)
 :void (event input-event))

(defgmethod
 (node+-shortcut-input :class 'node :bind "_shortcut_input" :hash 3754044979
  :virtual common-lisp:t)
 :void (event input-event))

(defgmethod
 (node+-unhandled-input :class 'node :bind "_unhandled_input" :hash 3754044979
  :virtual common-lisp:t)
 :void (event input-event))

(defgmethod
 (node+-unhandled-key-input :class 'node :bind "_unhandled_key_input" :hash
  3754044979 :virtual common-lisp:t)
 :void (event input-event))

(defgmethod
 (node+-get-focused-accessibility-element :class 'node :bind
  "_get_focused_accessibility_element" :hash 2944877500 :virtual common-lisp:t)
 rid)

(defgmethod
 (node+print-orphan-nodes :class 'node :bind "print_orphan_nodes" :hash
  3218959716 :static common-lisp:t)
 :void)

(defgmethod
 (node+get-orphan-node-ids :class 'node :bind "get_orphan_node_ids" :hash
  2915620761 :static common-lisp:t)
 array)

(defgmethod
 (node+add-sibling :class 'node :bind "add_sibling" :hash 2570952461) :void
 (sibling node) (force-readable-name bool))

(defgmethod (node+set-name :class 'node :bind "set_name" :hash 3304788590)
 :void (name string-name))

(defgmethod (node+get-name :class 'node :bind "get_name" :hash 2002593661)
 string-name)

(defgmethod (node+add-child :class 'node :bind "add_child" :hash 3863233950)
 :void (node node) (force-readable-name bool) (internal node+internal-mode))

(defgmethod
 (node+remove-child :class 'node :bind "remove_child" :hash 1078189570) :void
 (node node))

(defgmethod (node+reparent :class 'node :bind "reparent" :hash 3685795103)
 :void (new-parent node) (keep-global-transform bool))

(defgmethod
 (node+get-child-count :class 'node :bind "get_child_count" :hash 894402480)
 int (include-internal bool))

(defgmethod
 (node+get-children :class 'node :bind "get_children" :hash 873284517) array
 (include-internal bool))

(defgmethod (node+get-child :class 'node :bind "get_child" :hash 541253412)
 node (idx int) (include-internal bool))

(defgmethod (node+has-node :class 'node :bind "has_node" :hash 861721659) bool
 (path node-path))

(defgmethod (node+get-node :class 'node :bind "get_node" :hash 2734337346) node
 (path node-path))

(defgmethod
 (node+get-node-or-null :class 'node :bind "get_node_or_null" :hash 2734337346)
 node (path node-path))

(defgmethod (node+get-parent :class 'node :bind "get_parent" :hash 3160264692)
 node)

(defgmethod (node+find-child :class 'node :bind "find_child" :hash 2008217037)
 node (pattern string) (recursive bool) (owned bool))

(defgmethod
 (node+find-children :class 'node :bind "find_children" :hash 2560337219) array
 (pattern string) (type string) (recursive bool) (owned bool))

(defgmethod
 (node+find-parent :class 'node :bind "find_parent" :hash 1140089439) node
 (pattern string))

(defgmethod
 (node+has-node-and-resource :class 'node :bind "has_node_and_resource" :hash
  861721659)
 bool (path node-path))

(defgmethod
 (node+get-node-and-resource :class 'node :bind "get_node_and_resource" :hash
  502563882)
 array (path node-path))

(defgmethod
 (node+is-inside-tree :class 'node :bind "is_inside_tree" :hash 36873697) bool)

(defgmethod
 (node+is-part-of-edited-scene :class 'node :bind "is_part_of_edited_scene"
  :hash 36873697)
 bool)

(defgmethod
 (node+is-ancestor-of :class 'node :bind "is_ancestor_of" :hash 3093956946)
 bool (node node))

(defgmethod
 (node+is-greater-than :class 'node :bind "is_greater_than" :hash 3093956946)
 bool (node node))

(defgmethod (node+get-path :class 'node :bind "get_path" :hash 4075236667)
 node-path)

(defgmethod (node+get-path-to :class 'node :bind "get_path_to" :hash 498846349)
 node-path (node node) (use-unique-path bool))

(defgmethod
 (node+add-to-group :class 'node :bind "add_to_group" :hash 3683006648) :void
 (group string-name) (persistent bool))

(defgmethod
 (node+remove-from-group :class 'node :bind "remove_from_group" :hash
  3304788590)
 :void (group string-name))

(defgmethod
 (node+is-in-group :class 'node :bind "is_in_group" :hash 2619796661) bool
 (group string-name))

(defgmethod (node+move-child :class 'node :bind "move_child" :hash 3315886247)
 :void (child-node node) (to-index int))

(defgmethod (node+get-groups :class 'node :bind "get_groups" :hash 3995934104)
 array)

(defgmethod (node+set-owner :class 'node :bind "set_owner" :hash 1078189570)
 :void (owner node))

(defgmethod (node+get-owner :class 'node :bind "get_owner" :hash 3160264692)
 node)

(defgmethod (node+get-index :class 'node :bind "get_index" :hash 894402480) int
 (include-internal bool))

(defgmethod (node+print-tree :class 'node :bind "print_tree" :hash 3218959716)
 :void)

(defgmethod
 (node+print-tree-pretty :class 'node :bind "print_tree_pretty" :hash
  3218959716)
 :void)

(defgmethod
 (node+get-tree-string :class 'node :bind "get_tree_string" :hash 2841200299)
 string)

(defgmethod
 (node+get-tree-string-pretty :class 'node :bind "get_tree_string_pretty" :hash
  2841200299)
 string)

(defgmethod
 (node+set-scene-file-path :class 'node :bind "set_scene_file_path" :hash
  83702148)
 :void (scene-file-path string))

(defgmethod
 (node+get-scene-file-path :class 'node :bind "get_scene_file_path" :hash
  201670096)
 string)

(defgmethod
 (node+propagate-notification :class 'node :bind "propagate_notification" :hash
  1286410249)
 :void (what int))

(defgmethod
 (node+propagate-call :class 'node :bind "propagate_call" :hash 1871007965)
 :void (method string-name) (args array) (parent-first bool))

(defgmethod
 (node+set-physics-process :class 'node :bind "set_physics_process" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (node+get-physics-process-delta-time :class 'node :bind
  "get_physics_process_delta_time" :hash 1740695150)
 float)

(defgmethod
 (node+is-physics-processing :class 'node :bind "is_physics_processing" :hash
  36873697)
 bool)

(defgmethod
 (node+get-process-delta-time :class 'node :bind "get_process_delta_time" :hash
  1740695150)
 float)

(defgmethod
 (node+set-process :class 'node :bind "set_process" :hash 2586408642) :void
 (enable bool))

(defgmethod
 (node+set-process-priority :class 'node :bind "set_process_priority" :hash
  1286410249)
 :void (priority int))

(defgmethod
 (node+get-process-priority :class 'node :bind "get_process_priority" :hash
  3905245786)
 int)

(defgmethod
 (node+set-physics-process-priority :class 'node :bind
  "set_physics_process_priority" :hash 1286410249)
 :void (priority int))

(defgmethod
 (node+get-physics-process-priority :class 'node :bind
  "get_physics_process_priority" :hash 3905245786)
 int)

(defgmethod
 (node+is-processing :class 'node :bind "is_processing" :hash 36873697) bool)

(defgmethod
 (node+set-process-input :class 'node :bind "set_process_input" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (node+is-processing-input :class 'node :bind "is_processing_input" :hash
  36873697)
 bool)

(defgmethod
 (node+set-process-shortcut-input :class 'node :bind
  "set_process_shortcut_input" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (node+is-processing-shortcut-input :class 'node :bind
  "is_processing_shortcut_input" :hash 36873697)
 bool)

(defgmethod
 (node+set-process-unhandled-input :class 'node :bind
  "set_process_unhandled_input" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (node+is-processing-unhandled-input :class 'node :bind
  "is_processing_unhandled_input" :hash 36873697)
 bool)

(defgmethod
 (node+set-process-unhandled-key-input :class 'node :bind
  "set_process_unhandled_key_input" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (node+is-processing-unhandled-key-input :class 'node :bind
  "is_processing_unhandled_key_input" :hash 36873697)
 bool)

(defgmethod
 (node+set-process-mode :class 'node :bind "set_process_mode" :hash 1841290486)
 :void (mode node+process-mode))

(defgmethod
 (node+get-process-mode :class 'node :bind "get_process_mode" :hash 739966102)
 node+process-mode)

(defgmethod (node+can-process :class 'node :bind "can_process" :hash 36873697)
 bool)

(defgmethod
 (node+set-process-thread-group :class 'node :bind "set_process_thread_group"
  :hash 2275442745)
 :void (mode node+process-thread-group))

(defgmethod
 (node+get-process-thread-group :class 'node :bind "get_process_thread_group"
  :hash 1866404740)
 node+process-thread-group)

(defgmethod
 (node+set-process-thread-messages :class 'node :bind
  "set_process_thread_messages" :hash 1357280998)
 :void (flags node+process-thread-messages))

(defgmethod
 (node+get-process-thread-messages :class 'node :bind
  "get_process_thread_messages" :hash 4228993612)
 node+process-thread-messages)

(defgmethod
 (node+set-process-thread-group-order :class 'node :bind
  "set_process_thread_group_order" :hash 1286410249)
 :void (order int))

(defgmethod
 (node+get-process-thread-group-order :class 'node :bind
  "get_process_thread_group_order" :hash 3905245786)
 int)

(defgmethod
 (node+queue-accessibility-update :class 'node :bind
  "queue_accessibility_update" :hash 3218959716)
 :void)

(defgmethod
 (node+get-accessibility-element :class 'node :bind "get_accessibility_element"
  :hash 2944877500)
 rid)

(defgmethod
 (node+set-display-folded :class 'node :bind "set_display_folded" :hash
  2586408642)
 :void (fold bool))

(defgmethod
 (node+is-displayed-folded :class 'node :bind "is_displayed_folded" :hash
  36873697)
 bool)

(defgmethod
 (node+set-process-internal :class 'node :bind "set_process_internal" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (node+is-processing-internal :class 'node :bind "is_processing_internal" :hash
  36873697)
 bool)

(defgmethod
 (node+set-physics-process-internal :class 'node :bind
  "set_physics_process_internal" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (node+is-physics-processing-internal :class 'node :bind
  "is_physics_processing_internal" :hash 36873697)
 bool)

(defgmethod
 (node+set-physics-interpolation-mode :class 'node :bind
  "set_physics_interpolation_mode" :hash 3202404928)
 :void (mode node+physics-interpolation-mode))

(defgmethod
 (node+get-physics-interpolation-mode :class 'node :bind
  "get_physics_interpolation_mode" :hash 2920385216)
 node+physics-interpolation-mode)

(defgmethod
 (node+is-physics-interpolated :class 'node :bind "is_physics_interpolated"
  :hash 36873697)
 bool)

(defgmethod
 (node+is-physics-interpolated-and-enabled :class 'node :bind
  "is_physics_interpolated_and_enabled" :hash 36873697)
 bool)

(defgmethod
 (node+reset-physics-interpolation :class 'node :bind
  "reset_physics_interpolation" :hash 3218959716)
 :void)

(defgmethod
 (node+set-auto-translate-mode :class 'node :bind "set_auto_translate_mode"
  :hash 776149714)
 :void (mode node+auto-translate-mode))

(defgmethod
 (node+get-auto-translate-mode :class 'node :bind "get_auto_translate_mode"
  :hash 2498906432)
 node+auto-translate-mode)

(defgmethod
 (node+can-auto-translate :class 'node :bind "can_auto_translate" :hash
  36873697)
 bool)

(defgmethod
 (node+set-translation-domain-inherited :class 'node :bind
  "set_translation_domain_inherited" :hash 3218959716)
 :void)

(defgmethod (node+get-window :class 'node :bind "get_window" :hash 1757182445)
 window)

(defgmethod
 (node+get-last-exclusive-window :class 'node :bind "get_last_exclusive_window"
  :hash 1757182445)
 window)

(defgmethod (node+get-tree :class 'node :bind "get_tree" :hash 2958820483)
 scene-tree)

(defgmethod
 (node+create-tween :class 'node :bind "create_tween" :hash 3426978995) tween)

(defgmethod (node+duplicate :class 'node :bind "duplicate" :hash 3511555459)
 node (flags int))

(defgmethod (node+replace-by :class 'node :bind "replace_by" :hash 2570952461)
 :void (node node) (keep-groups bool))

(defgmethod
 (node+set-scene-instance-load-placeholder :class 'node :bind
  "set_scene_instance_load_placeholder" :hash 2586408642)
 :void (load-placeholder bool))

(defgmethod
 (node+get-scene-instance-load-placeholder :class 'node :bind
  "get_scene_instance_load_placeholder" :hash 36873697)
 bool)

(defgmethod
 (node+set-editable-instance :class 'node :bind "set_editable_instance" :hash
  2731852923)
 :void (node node) (is-editable bool))

(defgmethod
 (node+is-editable-instance :class 'node :bind "is_editable_instance" :hash
  3093956946)
 bool (node node))

(defgmethod
 (node+get-viewport :class 'node :bind "get_viewport" :hash 3596683776)
 viewport)

(defgmethod (node+queue-free :class 'node :bind "queue_free" :hash 3218959716)
 :void)

(defgmethod
 (node+request-ready :class 'node :bind "request_ready" :hash 3218959716) :void)

(defgmethod
 (node+is-node-ready :class 'node :bind "is_node_ready" :hash 36873697) bool)

(defgmethod
 (node+set-multiplayer-authority :class 'node :bind "set_multiplayer_authority"
  :hash 972357352)
 :void (id int) (recursive bool))

(defgmethod
 (node+get-multiplayer-authority :class 'node :bind "get_multiplayer_authority"
  :hash 3905245786)
 int)

(defgmethod
 (node+is-multiplayer-authority :class 'node :bind "is_multiplayer_authority"
  :hash 36873697)
 bool)

(defgmethod
 (node+get-multiplayer :class 'node :bind "get_multiplayer" :hash 406750475)
 multiplayer-api)

(defgmethod (node+rpc-config :class 'node :bind "rpc_config" :hash 3776071444)
 :void (method string-name) (config variant))

(defgmethod
 (node+get-node-rpc-config :class 'node :bind "get_node_rpc_config" :hash
  1214101251)
 variant)

(defgmethod
 (node+set-editor-description :class 'node :bind "set_editor_description" :hash
  83702148)
 :void (editor-description string))

(defgmethod
 (node+get-editor-description :class 'node :bind "get_editor_description" :hash
  201670096)
 string)

(defgmethod
 (node+set-unique-name-in-owner :class 'node :bind "set_unique_name_in_owner"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (node+is-unique-name-in-owner :class 'node :bind "is_unique_name_in_owner"
  :hash 36873697)
 bool)

(defgmethod (node+atr :class 'node :bind "atr" :hash 3344478075) string
 (message string) (context string-name))

(defgmethod (node+atr-n :class 'node :bind "atr_n" :hash 259354841) string
 (message string) (plural-message string-name) (n int) (context string-name))

(defgmethod
 (node+rpc :class 'node :bind "rpc" :hash 4047867050 :vararg common-lisp:t)
 error (method string-name))

(defgmethod
 (node+rpc-id :class 'node :bind "rpc_id" :hash 361499283 :vararg
  common-lisp:t)
 error (peer-id int) (method string-name))

(defgmethod
 (node+update-configuration-warnings :class 'node :bind
  "update_configuration_warnings" :hash 3218959716)
 :void)

(defgmethod
 (node+call-deferred-thread-group :class 'node :bind
  "call_deferred_thread_group" :hash 3400424181 :vararg common-lisp:t)
 variant (method string-name))

(defgmethod
 (node+set-deferred-thread-group :class 'node :bind "set_deferred_thread_group"
  :hash 3776071444)
 :void (property string-name) (value variant))

(defgmethod
 (node+notify-deferred-thread-group :class 'node :bind
  "notify_deferred_thread_group" :hash 1286410249)
 :void (what int))

(defgmethod
 (node+call-thread-safe :class 'node :bind "call_thread_safe" :hash 3400424181
  :vararg common-lisp:t)
 variant (method string-name))

(defgmethod
 (node+set-thread-safe :class 'node :bind "set_thread_safe" :hash 3776071444)
 :void (property string-name) (value variant))

(defgmethod
 (node+notify-thread-safe :class 'node :bind "notify_thread_safe" :hash
  1286410249)
 :void (what int))