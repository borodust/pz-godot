(common-lisp:in-package :%godot)


(defgmethod
 (editor-plugin+%forward-canvas-gui-input :class 'editor-plugin :bind
  "_forward_canvas_gui_input" :hash 1062211774 :virtual common-lisp:t)
 bool (event input-event))

(defgmethod
 (editor-plugin+%forward-canvas-draw-over-viewport :class 'editor-plugin :bind
  "_forward_canvas_draw_over_viewport" :hash 1496901182 :virtual common-lisp:t)
 :void (viewport-control control))

(defgmethod
 (editor-plugin+%forward-canvas-force-draw-over-viewport :class 'editor-plugin
  :bind "_forward_canvas_force_draw_over_viewport" :hash 1496901182 :virtual
  common-lisp:t)
 :void (viewport-control control))

(defgmethod
 (editor-plugin+%forward-3d-gui-input :class 'editor-plugin :bind
  "_forward_3d_gui_input" :hash 1018736637 :virtual common-lisp:t)
 int (viewport-camera camera-3d) (event input-event))

(defgmethod
 (editor-plugin+%forward-3d-draw-over-viewport :class 'editor-plugin :bind
  "_forward_3d_draw_over_viewport" :hash 1496901182 :virtual common-lisp:t)
 :void (viewport-control control))

(defgmethod
 (editor-plugin+%forward-3d-force-draw-over-viewport :class 'editor-plugin
  :bind "_forward_3d_force_draw_over_viewport" :hash 1496901182 :virtual
  common-lisp:t)
 :void (viewport-control control))

(defgmethod
 (editor-plugin+%get-plugin-name :class 'editor-plugin :bind "_get_plugin_name"
  :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (editor-plugin+%get-plugin-icon :class 'editor-plugin :bind "_get_plugin_icon"
  :hash 3635182373 :virtual common-lisp:t)
 texture-2d)

(defgmethod
 (editor-plugin+%has-main-screen :class 'editor-plugin :bind "_has_main_screen"
  :hash 36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (editor-plugin+%make-visible :class 'editor-plugin :bind "_make_visible" :hash
  2586408642 :virtual common-lisp:t)
 :void (visible bool))

(defgmethod
 (editor-plugin+%edit :class 'editor-plugin :bind "_edit" :hash 3975164845
  :virtual common-lisp:t)
 :void (object object))

(defgmethod
 (editor-plugin+%handles :class 'editor-plugin :bind "_handles" :hash 397768994
  :virtual common-lisp:t)
 bool (object object))

(defgmethod
 (editor-plugin+%get-state :class 'editor-plugin :bind "_get_state" :hash
  3102165223 :virtual common-lisp:t)
 dictionary)

(defgmethod
 (editor-plugin+%set-state :class 'editor-plugin :bind "_set_state" :hash
  4155329257 :virtual common-lisp:t)
 :void (state dictionary))

(defgmethod
 (editor-plugin+%clear :class 'editor-plugin :bind "_clear" :hash 3218959716
  :virtual common-lisp:t)
 :void)

(defgmethod
 (editor-plugin+%get-unsaved-status :class 'editor-plugin :bind
  "_get_unsaved_status" :hash 3135753539 :virtual common-lisp:t)
 string (for-scene string))

(defgmethod
 (editor-plugin+%save-external-data :class 'editor-plugin :bind
  "_save_external_data" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (editor-plugin+%apply-changes :class 'editor-plugin :bind "_apply_changes"
  :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (editor-plugin+%get-breakpoints :class 'editor-plugin :bind "_get_breakpoints"
  :hash 1139954409 :virtual common-lisp:t)
 packed-string-array)

(defgmethod
 (editor-plugin+%set-window-layout :class 'editor-plugin :bind
  "_set_window_layout" :hash 853519107 :virtual common-lisp:t)
 :void (configuration config-file))

(defgmethod
 (editor-plugin+%get-window-layout :class 'editor-plugin :bind
  "_get_window_layout" :hash 853519107 :virtual common-lisp:t)
 :void (configuration config-file))

(defgmethod
 (editor-plugin+%build :class 'editor-plugin :bind "_build" :hash 2240911060
  :virtual common-lisp:t)
 bool)

(defgmethod
 (editor-plugin+%run-scene :class 'editor-plugin :bind "_run_scene" :hash
  3911848509 :virtual common-lisp:t)
 packed-string-array (scene string) (args packed-string-array))

(defgmethod
 (editor-plugin+%enable-plugin :class 'editor-plugin :bind "_enable_plugin"
  :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (editor-plugin+%disable-plugin :class 'editor-plugin :bind "_disable_plugin"
  :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (editor-plugin+add-dock :class 'editor-plugin :bind "add_dock" :hash
  158651717)
 :void (dock editor-dock))

(defgmethod
 (editor-plugin+remove-dock :class 'editor-plugin :bind "remove_dock" :hash
  158651717)
 :void (dock editor-dock))

(defgmethod
 (editor-plugin+add-control-to-container :class 'editor-plugin :bind
  "add_control_to_container" :hash 3092750152)
 :void (container editor-plugin+custom-control-container) (control control))

(defgmethod
 (editor-plugin+remove-control-from-container :class 'editor-plugin :bind
  "remove_control_from_container" :hash 3092750152)
 :void (container editor-plugin+custom-control-container) (control control))

(defgmethod
 (editor-plugin+add-tool-menu-item :class 'editor-plugin :bind
  "add_tool_menu_item" :hash 2137474292)
 :void (name string) (callable callable))

(defgmethod
 (editor-plugin+add-tool-submenu-item :class 'editor-plugin :bind
  "add_tool_submenu_item" :hash 1019428915)
 :void (name string) (submenu popup-menu))

(defgmethod
 (editor-plugin+remove-tool-menu-item :class 'editor-plugin :bind
  "remove_tool_menu_item" :hash 83702148)
 :void (name string))

(defgmethod
 (editor-plugin+get-export-as-menu :class 'editor-plugin :bind
  "get_export_as_menu" :hash 1775878644)
 popup-menu)

(defgmethod
 (editor-plugin+add-custom-type :class 'editor-plugin :bind "add_custom_type"
  :hash 1986814599)
 :void (type string) (base string) (script script) (icon texture-2d))

(defgmethod
 (editor-plugin+remove-custom-type :class 'editor-plugin :bind
  "remove_custom_type" :hash 83702148)
 :void (type string))

(defgmethod
 (editor-plugin+add-control-to-dock :class 'editor-plugin :bind
  "add_control_to_dock" :hash 2994930786)
 :void (slot editor-plugin+dock-slot) (control control) (shortcut shortcut))

(defgmethod
 (editor-plugin+remove-control-from-docks :class 'editor-plugin :bind
  "remove_control_from_docks" :hash 1496901182)
 :void (control control))

(defgmethod
 (editor-plugin+set-dock-tab-icon :class 'editor-plugin :bind
  "set_dock_tab_icon" :hash 3450529724)
 :void (control control) (icon texture-2d))

(defgmethod
 (editor-plugin+add-control-to-bottom-panel :class 'editor-plugin :bind
  "add_control_to_bottom_panel" :hash 111032269)
 button (control control) (title string) (shortcut shortcut))

(defgmethod
 (editor-plugin+remove-control-from-bottom-panel :class 'editor-plugin :bind
  "remove_control_from_bottom_panel" :hash 1496901182)
 :void (control control))

(defgmethod
 (editor-plugin+add-autoload-singleton :class 'editor-plugin :bind
  "add_autoload_singleton" :hash 3186203200)
 :void (name string) (path string))

(defgmethod
 (editor-plugin+remove-autoload-singleton :class 'editor-plugin :bind
  "remove_autoload_singleton" :hash 83702148)
 :void (name string))

(defgmethod
 (editor-plugin+update-overlays :class 'editor-plugin :bind "update_overlays"
  :hash 3905245786)
 int)

(defgmethod
 (editor-plugin+make-bottom-panel-item-visible :class 'editor-plugin :bind
  "make_bottom_panel_item_visible" :hash 1496901182)
 :void (item control))

(defgmethod
 (editor-plugin+hide-bottom-panel :class 'editor-plugin :bind
  "hide_bottom_panel" :hash 3218959716)
 :void)

(defgmethod
 (editor-plugin+get-undo-redo :class 'editor-plugin :bind "get_undo_redo" :hash
  773492341)
 editor-undo-redo-manager)

(defgmethod
 (editor-plugin+add-undo-redo-inspector-hook-callback :class 'editor-plugin
  :bind "add_undo_redo_inspector_hook_callback" :hash 1611583062)
 :void (callable callable))

(defgmethod
 (editor-plugin+remove-undo-redo-inspector-hook-callback :class 'editor-plugin
  :bind "remove_undo_redo_inspector_hook_callback" :hash 1611583062)
 :void (callable callable))

(defgmethod
 (editor-plugin+queue-save-layout :class 'editor-plugin :bind
  "queue_save_layout" :hash 3218959716)
 :void)

(defgmethod
 (editor-plugin+add-translation-parser-plugin :class 'editor-plugin :bind
  "add_translation_parser_plugin" :hash 3116463128)
 :void (parser editor-translation-parser-plugin))

(defgmethod
 (editor-plugin+remove-translation-parser-plugin :class 'editor-plugin :bind
  "remove_translation_parser_plugin" :hash 3116463128)
 :void (parser editor-translation-parser-plugin))

(defgmethod
 (editor-plugin+add-import-plugin :class 'editor-plugin :bind
  "add_import_plugin" :hash 3113975762)
 :void (importer editor-import-plugin) (first-priority bool))

(defgmethod
 (editor-plugin+remove-import-plugin :class 'editor-plugin :bind
  "remove_import_plugin" :hash 2312482773)
 :void (importer editor-import-plugin))

(defgmethod
 (editor-plugin+add-scene-format-importer-plugin :class 'editor-plugin :bind
  "add_scene_format_importer_plugin" :hash 2764104752)
 :void (scene-format-importer editor-scene-format-importer)
 (first-priority bool))

(defgmethod
 (editor-plugin+remove-scene-format-importer-plugin :class 'editor-plugin :bind
  "remove_scene_format_importer_plugin" :hash 2637776123)
 :void (scene-format-importer editor-scene-format-importer))

(defgmethod
 (editor-plugin+add-scene-post-import-plugin :class 'editor-plugin :bind
  "add_scene_post_import_plugin" :hash 3492436322)
 :void (scene-import-plugin editor-scene-post-import-plugin)
 (first-priority bool))

(defgmethod
 (editor-plugin+remove-scene-post-import-plugin :class 'editor-plugin :bind
  "remove_scene_post_import_plugin" :hash 3045178206)
 :void (scene-import-plugin editor-scene-post-import-plugin))

(defgmethod
 (editor-plugin+add-export-plugin :class 'editor-plugin :bind
  "add_export_plugin" :hash 4095952207)
 :void (plugin editor-export-plugin))

(defgmethod
 (editor-plugin+remove-export-plugin :class 'editor-plugin :bind
  "remove_export_plugin" :hash 4095952207)
 :void (plugin editor-export-plugin))

(defgmethod
 (editor-plugin+add-export-platform :class 'editor-plugin :bind
  "add_export_platform" :hash 3431312373)
 :void (platform editor-export-platform))

(defgmethod
 (editor-plugin+remove-export-platform :class 'editor-plugin :bind
  "remove_export_platform" :hash 3431312373)
 :void (platform editor-export-platform))

(defgmethod
 (editor-plugin+add-node-3d-gizmo-plugin :class 'editor-plugin :bind
  "add_node_3d_gizmo_plugin" :hash 1541015022)
 :void (plugin editor-node-3dgizmo-plugin))

(defgmethod
 (editor-plugin+remove-node-3d-gizmo-plugin :class 'editor-plugin :bind
  "remove_node_3d_gizmo_plugin" :hash 1541015022)
 :void (plugin editor-node-3dgizmo-plugin))

(defgmethod
 (editor-plugin+add-inspector-plugin :class 'editor-plugin :bind
  "add_inspector_plugin" :hash 546395733)
 :void (plugin editor-inspector-plugin))

(defgmethod
 (editor-plugin+remove-inspector-plugin :class 'editor-plugin :bind
  "remove_inspector_plugin" :hash 546395733)
 :void (plugin editor-inspector-plugin))

(defgmethod
 (editor-plugin+add-resource-conversion-plugin :class 'editor-plugin :bind
  "add_resource_conversion_plugin" :hash 2124849111)
 :void (plugin editor-resource-conversion-plugin))

(defgmethod
 (editor-plugin+remove-resource-conversion-plugin :class 'editor-plugin :bind
  "remove_resource_conversion_plugin" :hash 2124849111)
 :void (plugin editor-resource-conversion-plugin))

(defgmethod
 (editor-plugin+set-input-event-forwarding-always-enabled :class 'editor-plugin
  :bind "set_input_event_forwarding_always_enabled" :hash 3218959716)
 :void)

(defgmethod
 (editor-plugin+set-force-draw-over-forwarding-enabled :class 'editor-plugin
  :bind "set_force_draw_over_forwarding_enabled" :hash 3218959716)
 :void)

(defgmethod
 (editor-plugin+add-context-menu-plugin :class 'editor-plugin :bind
  "add_context_menu_plugin" :hash 1904221872)
 :void (slot editor-context-menu-plugin+context-menu-slot)
 (plugin editor-context-menu-plugin))

(defgmethod
 (editor-plugin+remove-context-menu-plugin :class 'editor-plugin :bind
  "remove_context_menu_plugin" :hash 2281511854)
 :void (plugin editor-context-menu-plugin))

(defgmethod
 (editor-plugin+get-editor-interface :class 'editor-plugin :bind
  "get_editor_interface" :hash 4223731786)
 editor-interface)

(defgmethod
 (editor-plugin+get-script-create-dialog :class 'editor-plugin :bind
  "get_script_create_dialog" :hash 3121871482)
 script-create-dialog)

(defgmethod
 (editor-plugin+add-debugger-plugin :class 'editor-plugin :bind
  "add_debugger_plugin" :hash 3749880309)
 :void (script editor-debugger-plugin))

(defgmethod
 (editor-plugin+remove-debugger-plugin :class 'editor-plugin :bind
  "remove_debugger_plugin" :hash 3749880309)
 :void (script editor-debugger-plugin))

(defgmethod
 (editor-plugin+get-plugin-version :class 'editor-plugin :bind
  "get_plugin_version" :hash 201670096)
 string)