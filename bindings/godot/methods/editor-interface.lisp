(common-lisp:in-package :%godot)


(defgmethod
 (editor-interface+restart-editor :class 'editor-interface :bind
  "restart_editor" :hash 3216645846)
 :void (save bool))

(defgmethod
 (editor-interface+get-command-palette :class 'editor-interface :bind
  "get_command_palette" :hash 2471163807)
 editor-command-palette)

(defgmethod
 (editor-interface+get-resource-filesystem :class 'editor-interface :bind
  "get_resource_filesystem" :hash 780151678)
 editor-file-system)

(defgmethod
 (editor-interface+get-editor-paths :class 'editor-interface :bind
  "get_editor_paths" :hash 1595760068)
 editor-paths)

(defgmethod
 (editor-interface+get-resource-previewer :class 'editor-interface :bind
  "get_resource_previewer" :hash 943486957)
 editor-resource-preview)

(defgmethod
 (editor-interface+get-selection :class 'editor-interface :bind "get_selection"
  :hash 2690272531)
 editor-selection)

(defgmethod
 (editor-interface+get-editor-settings :class 'editor-interface :bind
  "get_editor_settings" :hash 4086932459)
 editor-settings)

(defgmethod
 (editor-interface+get-editor-toaster :class 'editor-interface :bind
  "get_editor_toaster" :hash 3612675797)
 editor-toaster)

(defgmethod
 (editor-interface+get-editor-undo-redo :class 'editor-interface :bind
  "get_editor_undo_redo" :hash 3819628421)
 editor-undo-redo-manager)

(defgmethod
 (editor-interface+make-mesh-previews :class 'editor-interface :bind
  "make_mesh_previews" :hash 878078554)
 array (meshes array) (preview-size int))

(defgmethod
 (editor-interface+set-plugin-enabled :class 'editor-interface :bind
  "set_plugin_enabled" :hash 2678287736)
 :void (plugin string) (enabled bool))

(defgmethod
 (editor-interface+is-plugin-enabled :class 'editor-interface :bind
  "is_plugin_enabled" :hash 3927539163)
 bool (plugin string))

(defgmethod
 (editor-interface+get-editor-theme :class 'editor-interface :bind
  "get_editor_theme" :hash 3846893731)
 theme)

(defgmethod
 (editor-interface+get-base-control :class 'editor-interface :bind
  "get_base_control" :hash 2783021301)
 control)

(defgmethod
 (editor-interface+get-editor-main-screen :class 'editor-interface :bind
  "get_editor_main_screen" :hash 1706218421)
 vbox-container)

(defgmethod
 (editor-interface+get-script-editor :class 'editor-interface :bind
  "get_script_editor" :hash 90868003)
 script-editor)

(defgmethod
 (editor-interface+get-editor-viewport-2d :class 'editor-interface :bind
  "get_editor_viewport_2d" :hash 3750751911)
 sub-viewport)

(defgmethod
 (editor-interface+get-editor-viewport-3d :class 'editor-interface :bind
  "get_editor_viewport_3d" :hash 1970834490)
 sub-viewport (idx int))

(defgmethod
 (editor-interface+set-main-screen-editor :class 'editor-interface :bind
  "set_main_screen_editor" :hash 83702148)
 :void (name string))

(defgmethod
 (editor-interface+set-distraction-free-mode :class 'editor-interface :bind
  "set_distraction_free_mode" :hash 2586408642)
 :void (enter bool))

(defgmethod
 (editor-interface+is-distraction-free-mode-enabled :class 'editor-interface
  :bind "is_distraction_free_mode_enabled" :hash 36873697)
 bool)

(defgmethod
 (editor-interface+is-multi-window-enabled :class 'editor-interface :bind
  "is_multi_window_enabled" :hash 36873697)
 bool)

(defgmethod
 (editor-interface+get-editor-scale :class 'editor-interface :bind
  "get_editor_scale" :hash 1740695150)
 float)

(defgmethod
 (editor-interface+get-editor-language :class 'editor-interface :bind
  "get_editor_language" :hash 201670096)
 string)

(defgmethod
 (editor-interface+is-node-3d-snap-enabled :class 'editor-interface :bind
  "is_node_3d_snap_enabled" :hash 36873697)
 bool)

(defgmethod
 (editor-interface+get-node-3d-translate-snap :class 'editor-interface :bind
  "get_node_3d_translate_snap" :hash 1740695150)
 float)

(defgmethod
 (editor-interface+get-node-3d-rotate-snap :class 'editor-interface :bind
  "get_node_3d_rotate_snap" :hash 1740695150)
 float)

(defgmethod
 (editor-interface+get-node-3d-scale-snap :class 'editor-interface :bind
  "get_node_3d_scale_snap" :hash 1740695150)
 float)

(defgmethod
 (editor-interface+popup-dialog :class 'editor-interface :bind "popup_dialog"
  :hash 2015770942)
 :void (dialog window) (rect rect-2i))

(defgmethod
 (editor-interface+popup-dialog-centered :class 'editor-interface :bind
  "popup_dialog_centered" :hash 346557367)
 :void (dialog window) (minsize vector-2i))

(defgmethod
 (editor-interface+popup-dialog-centered-ratio :class 'editor-interface :bind
  "popup_dialog_centered_ratio" :hash 2093669136)
 :void (dialog window) (ratio float))

(defgmethod
 (editor-interface+popup-dialog-centered-clamped :class 'editor-interface :bind
  "popup_dialog_centered_clamped" :hash 3763385571)
 :void (dialog window) (minsize vector-2i) (fallback-ratio float))

(defgmethod
 (editor-interface+get-current-feature-profile :class 'editor-interface :bind
  "get_current_feature_profile" :hash 201670096)
 string)

(defgmethod
 (editor-interface+set-current-feature-profile :class 'editor-interface :bind
  "set_current_feature_profile" :hash 83702148)
 :void (profile-name string))

(defgmethod
 (editor-interface+popup-node-selector :class 'editor-interface :bind
  "popup_node_selector" :hash 2444591477)
 :void (callback callable) (valid-types array) (current-value node))

(defgmethod
 (editor-interface+popup-property-selector :class 'editor-interface :bind
  "popup_property_selector" :hash 2955609011)
 :void (object object) (callback callable) (type-filter packed-int-32array)
 (current-value string))

(defgmethod
 (editor-interface+popup-method-selector :class 'editor-interface :bind
  "popup_method_selector" :hash 3585505226)
 :void (object object) (callback callable) (current-value string))

(defgmethod
 (editor-interface+popup-quick-open :class 'editor-interface :bind
  "popup_quick_open" :hash 2271411043)
 :void (callback callable) (base-types array))

(defgmethod
 (editor-interface+popup-create-dialog :class 'editor-interface :bind
  "popup_create_dialog" :hash 495277124)
 :void (callback callable) (base-type string-name) (current-type string)
 (dialog-title string) (type-blocklist array))

(defgmethod
 (editor-interface+get-file-system-dock :class 'editor-interface :bind
  "get_file_system_dock" :hash 3751012327)
 file-system-dock)

(defgmethod
 (editor-interface+select-file :class 'editor-interface :bind "select_file"
  :hash 83702148)
 :void (file string))

(defgmethod
 (editor-interface+get-selected-paths :class 'editor-interface :bind
  "get_selected_paths" :hash 1139954409)
 packed-string-array)

(defgmethod
 (editor-interface+get-current-path :class 'editor-interface :bind
  "get_current_path" :hash 201670096)
 string)

(defgmethod
 (editor-interface+get-current-directory :class 'editor-interface :bind
  "get_current_directory" :hash 201670096)
 string)

(defgmethod
 (editor-interface+get-inspector :class 'editor-interface :bind "get_inspector"
  :hash 3517113938)
 editor-inspector)

(defgmethod
 (editor-interface+inspect-object :class 'editor-interface :bind
  "inspect_object" :hash 127962172)
 :void (object object) (for-property string) (inspector-only bool))

(defgmethod
 (editor-interface+edit-resource :class 'editor-interface :bind "edit_resource"
  :hash 968641751)
 :void (resource resource))

(defgmethod
 (editor-interface+edit-node :class 'editor-interface :bind "edit_node" :hash
  1078189570)
 :void (node node))

(defgmethod
 (editor-interface+edit-script :class 'editor-interface :bind "edit_script"
  :hash 219829402)
 :void (script script) (line int) (column int) (grab-focus bool))

(defgmethod
 (editor-interface+open-scene-from-path :class 'editor-interface :bind
  "open_scene_from_path" :hash 1168363258)
 :void (scene-filepath string) (set-inherited bool))

(defgmethod
 (editor-interface+reload-scene-from-path :class 'editor-interface :bind
  "reload_scene_from_path" :hash 83702148)
 :void (scene-filepath string))

(defgmethod
 (editor-interface+set-object-edited :class 'editor-interface :bind
  "set_object_edited" :hash 1462101905)
 :void (object object) (edited bool))

(defgmethod
 (editor-interface+is-object-edited :class 'editor-interface :bind
  "is_object_edited" :hash 397768994)
 bool (object object))

(defgmethod
 (editor-interface+get-open-scenes :class 'editor-interface :bind
  "get_open_scenes" :hash 1139954409)
 packed-string-array)

(defgmethod
 (editor-interface+get-unsaved-scenes :class 'editor-interface :bind
  "get_unsaved_scenes" :hash 1139954409)
 packed-string-array)

(defgmethod
 (editor-interface+get-open-scene-roots :class 'editor-interface :bind
  "get_open_scene_roots" :hash 3995934104)
 array)

(defgmethod
 (editor-interface+get-edited-scene-root :class 'editor-interface :bind
  "get_edited_scene_root" :hash 3160264692)
 node)

(defgmethod
 (editor-interface+add-root-node :class 'editor-interface :bind "add_root_node"
  :hash 1078189570)
 :void (node node))

(defgmethod
 (editor-interface+save-scene :class 'editor-interface :bind "save_scene" :hash
  166280745)
 error)

(defgmethod
 (editor-interface+save-scene-as :class 'editor-interface :bind "save_scene_as"
  :hash 3647332257)
 :void (path string) (with-preview bool))

(defgmethod
 (editor-interface+save-all-scenes :class 'editor-interface :bind
  "save_all_scenes" :hash 3218959716)
 :void)

(defgmethod
 (editor-interface+close-scene :class 'editor-interface :bind "close_scene"
  :hash 166280745)
 error)

(defgmethod
 (editor-interface+mark-scene-as-unsaved :class 'editor-interface :bind
  "mark_scene_as_unsaved" :hash 3218959716)
 :void)

(defgmethod
 (editor-interface+play-main-scene :class 'editor-interface :bind
  "play_main_scene" :hash 3218959716)
 :void)

(defgmethod
 (editor-interface+play-current-scene :class 'editor-interface :bind
  "play_current_scene" :hash 3218959716)
 :void)

(defgmethod
 (editor-interface+play-custom-scene :class 'editor-interface :bind
  "play_custom_scene" :hash 83702148)
 :void (scene-filepath string))

(defgmethod
 (editor-interface+stop-playing-scene :class 'editor-interface :bind
  "stop_playing_scene" :hash 3218959716)
 :void)

(defgmethod
 (editor-interface+is-playing-scene :class 'editor-interface :bind
  "is_playing_scene" :hash 36873697)
 bool)

(defgmethod
 (editor-interface+get-playing-scene :class 'editor-interface :bind
  "get_playing_scene" :hash 201670096)
 string)

(defgmethod
 (editor-interface+set-movie-maker-enabled :class 'editor-interface :bind
  "set_movie_maker_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (editor-interface+is-movie-maker-enabled :class 'editor-interface :bind
  "is_movie_maker_enabled" :hash 36873697)
 bool)