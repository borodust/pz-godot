(common-lisp:in-package :%godot)


(defgmethod
 (control+-has-point :class 'control :bind "_has_point" :hash 556197845
  :virtual common-lisp:t)
 bool (point vector-2))

(defgmethod
 (control+-structured-text-parser :class 'control :bind
  "_structured_text_parser" :hash 1292548940 :virtual common-lisp:t)
 array (args array) (text string))

(defgmethod
 (control+-get-minimum-size :class 'control :bind "_get_minimum_size" :hash
  3341600327 :virtual common-lisp:t)
 vector-2)

(defgmethod
 (control+-get-tooltip :class 'control :bind "_get_tooltip" :hash 3674420000
  :virtual common-lisp:t)
 string (at-position vector-2))

(defgmethod
 (control+-get-drag-data :class 'control :bind "_get_drag_data" :hash
  2233896889 :virtual common-lisp:t)
 variant (at-position vector-2))

(defgmethod
 (control+-can-drop-data :class 'control :bind "_can_drop_data" :hash
  2603004011 :virtual common-lisp:t)
 bool (at-position vector-2) (data variant))

(defgmethod
 (control+-drop-data :class 'control :bind "_drop_data" :hash 3699746064
  :virtual common-lisp:t)
 :void (at-position vector-2) (data variant))

(defgmethod
 (control+-make-custom-tooltip :class 'control :bind "_make_custom_tooltip"
  :hash 1976279298 :virtual common-lisp:t)
 object (for-text string))

(defgmethod
 (control+-accessibility-get-contextual-info :class 'control :bind
  "_accessibility_get_contextual_info" :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (control+-get-accessibility-container-name :class 'control :bind
  "_get_accessibility_container_name" :hash 2174079723 :virtual common-lisp:t)
 string (node node))

(defgmethod
 (control+-gui-input :class 'control :bind "_gui_input" :hash 3754044979
  :virtual common-lisp:t)
 :void (event input-event))

(defgmethod
 (control+accept-event :class 'control :bind "accept_event" :hash 3218959716)
 :void)

(defgmethod
 (control+get-minimum-size :class 'control :bind "get_minimum_size" :hash
  3341600327)
 vector-2)

(defgmethod
 (control+get-combined-minimum-size :class 'control :bind
  "get_combined_minimum_size" :hash 3341600327)
 vector-2)

(defgmethod
 (control+set-anchors-preset :class 'control :bind "set_anchors_preset" :hash
  509135270)
 :void (preset control+layout-preset) (keep-offsets bool))

(defgmethod
 (control+set-offsets-preset :class 'control :bind "set_offsets_preset" :hash
  3724524307)
 :void (preset control+layout-preset) (resize-mode control+layout-preset-mode)
 (margin int))

(defgmethod
 (control+set-anchors-and-offsets-preset :class 'control :bind
  "set_anchors_and_offsets_preset" :hash 3724524307)
 :void (preset control+layout-preset) (resize-mode control+layout-preset-mode)
 (margin int))

(defgmethod
 (control+set-anchor :class 'control :bind "set_anchor" :hash 2302782885) :void
 (side side) (anchor float) (keep-offset bool) (push-opposite-anchor bool))

(defgmethod
 (control+get-anchor :class 'control :bind "get_anchor" :hash 2869120046) float
 (side side))

(defgmethod
 (control+set-offset :class 'control :bind "set_offset" :hash 4290182280) :void
 (side side) (offset float))

(defgmethod
 (control+get-offset :class 'control :bind "get_offset" :hash 2869120046) float
 (offset side))

(defgmethod
 (control+set-anchor-and-offset :class 'control :bind "set_anchor_and_offset"
  :hash 4031722181)
 :void (side side) (anchor float) (offset float) (push-opposite-anchor bool))

(defgmethod
 (control+set-begin :class 'control :bind "set_begin" :hash 743155724) :void
 (position vector-2))

(defgmethod (control+set-end :class 'control :bind "set_end" :hash 743155724)
 :void (position vector-2))

(defgmethod
 (control+set-position :class 'control :bind "set_position" :hash 2436320129)
 :void (position vector-2) (keep-offsets bool))

(defgmethod
 (control+set-size :class 'control :bind "set_size" :hash 2436320129) :void
 (size vector-2) (keep-offsets bool))

(defgmethod
 (control+reset-size :class 'control :bind "reset_size" :hash 3218959716) :void)

(defgmethod
 (control+set-custom-minimum-size :class 'control :bind
  "set_custom_minimum_size" :hash 743155724)
 :void (size vector-2))

(defgmethod
 (control+set-global-position :class 'control :bind "set_global_position" :hash
  2436320129)
 :void (position vector-2) (keep-offsets bool))

(defgmethod
 (control+set-rotation :class 'control :bind "set_rotation" :hash 373806689)
 :void (radians float))

(defgmethod
 (control+set-rotation-degrees :class 'control :bind "set_rotation_degrees"
  :hash 373806689)
 :void (degrees float))

(defgmethod
 (control+set-scale :class 'control :bind "set_scale" :hash 743155724) :void
 (scale vector-2))

(defgmethod
 (control+set-pivot-offset :class 'control :bind "set_pivot_offset" :hash
  743155724)
 :void (pivot-offset vector-2))

(defgmethod
 (control+set-pivot-offset-ratio :class 'control :bind "set_pivot_offset_ratio"
  :hash 743155724)
 :void (ratio vector-2))

(defgmethod
 (control+get-begin :class 'control :bind "get_begin" :hash 3341600327)
 vector-2)

(defgmethod (control+get-end :class 'control :bind "get_end" :hash 3341600327)
 vector-2)

(defgmethod
 (control+get-position :class 'control :bind "get_position" :hash 3341600327)
 vector-2)

(defgmethod
 (control+get-size :class 'control :bind "get_size" :hash 3341600327) vector-2)

(defgmethod
 (control+get-rotation :class 'control :bind "get_rotation" :hash 1740695150)
 float)

(defgmethod
 (control+get-rotation-degrees :class 'control :bind "get_rotation_degrees"
  :hash 1740695150)
 float)

(defgmethod
 (control+get-scale :class 'control :bind "get_scale" :hash 3341600327)
 vector-2)

(defgmethod
 (control+get-pivot-offset :class 'control :bind "get_pivot_offset" :hash
  3341600327)
 vector-2)

(defgmethod
 (control+get-pivot-offset-ratio :class 'control :bind "get_pivot_offset_ratio"
  :hash 3341600327)
 vector-2)

(defgmethod
 (control+get-combined-pivot-offset :class 'control :bind
  "get_combined_pivot_offset" :hash 3341600327)
 vector-2)

(defgmethod
 (control+get-custom-minimum-size :class 'control :bind
  "get_custom_minimum_size" :hash 3341600327)
 vector-2)

(defgmethod
 (control+get-parent-area-size :class 'control :bind "get_parent_area_size"
  :hash 3341600327)
 vector-2)

(defgmethod
 (control+get-global-position :class 'control :bind "get_global_position" :hash
  3341600327)
 vector-2)

(defgmethod
 (control+get-screen-position :class 'control :bind "get_screen_position" :hash
  3341600327)
 vector-2)

(defgmethod
 (control+get-rect :class 'control :bind "get_rect" :hash 1639390495) rect-2)

(defgmethod
 (control+get-global-rect :class 'control :bind "get_global_rect" :hash
  1639390495)
 rect-2)

(defgmethod
 (control+set-focus-mode :class 'control :bind "set_focus_mode" :hash
  3232914922)
 :void (mode control+focus-mode))

(defgmethod
 (control+get-focus-mode :class 'control :bind "get_focus_mode" :hash
  2132829277)
 control+focus-mode)

(defgmethod
 (control+get-focus-mode-with-override :class 'control :bind
  "get_focus_mode_with_override" :hash 2132829277)
 control+focus-mode)

(defgmethod
 (control+set-focus-behavior-recursive :class 'control :bind
  "set_focus_behavior_recursive" :hash 4256832521)
 :void (focus-behavior-recursive control+focus-behavior-recursive))

(defgmethod
 (control+get-focus-behavior-recursive :class 'control :bind
  "get_focus_behavior_recursive" :hash 2435707181)
 control+focus-behavior-recursive)

(defgmethod
 (control+has-focus :class 'control :bind "has_focus" :hash 3302206351) bool
 (ignore-hidden-focus bool))

(defgmethod
 (control+grab-focus :class 'control :bind "grab_focus" :hash 107499316) :void
 (hide-focus bool))

(defgmethod
 (control+release-focus :class 'control :bind "release_focus" :hash 3218959716)
 :void)

(defgmethod
 (control+find-prev-valid-focus :class 'control :bind "find_prev_valid_focus"
  :hash 2783021301)
 control)

(defgmethod
 (control+find-next-valid-focus :class 'control :bind "find_next_valid_focus"
  :hash 2783021301)
 control)

(defgmethod
 (control+find-valid-focus-neighbor :class 'control :bind
  "find_valid_focus_neighbor" :hash 1543910170)
 control (side side))

(defgmethod
 (control+set-h-size-flags :class 'control :bind "set_h_size_flags" :hash
  394851643)
 :void (flags control+size-flags))

(defgmethod
 (control+get-h-size-flags :class 'control :bind "get_h_size_flags" :hash
  3781367401)
 control+size-flags)

(defgmethod
 (control+set-stretch-ratio :class 'control :bind "set_stretch_ratio" :hash
  373806689)
 :void (ratio float))

(defgmethod
 (control+get-stretch-ratio :class 'control :bind "get_stretch_ratio" :hash
  1740695150)
 float)

(defgmethod
 (control+set-v-size-flags :class 'control :bind "set_v_size_flags" :hash
  394851643)
 :void (flags control+size-flags))

(defgmethod
 (control+get-v-size-flags :class 'control :bind "get_v_size_flags" :hash
  3781367401)
 control+size-flags)

(defgmethod
 (control+set-theme :class 'control :bind "set_theme" :hash 2326690814) :void
 (theme theme))

(defgmethod
 (control+get-theme :class 'control :bind "get_theme" :hash 3846893731) theme)

(defgmethod
 (control+set-theme-type-variation :class 'control :bind
  "set_theme_type_variation" :hash 3304788590)
 :void (theme-type string-name))

(defgmethod
 (control+get-theme-type-variation :class 'control :bind
  "get_theme_type_variation" :hash 2002593661)
 string-name)

(defgmethod
 (control+begin-bulk-theme-override :class 'control :bind
  "begin_bulk_theme_override" :hash 3218959716)
 :void)

(defgmethod
 (control+end-bulk-theme-override :class 'control :bind
  "end_bulk_theme_override" :hash 3218959716)
 :void)

(defgmethod
 (control+add-theme-icon-override :class 'control :bind
  "add_theme_icon_override" :hash 1373065600)
 :void (name string-name) (texture texture-2d))

(defgmethod
 (control+add-theme-stylebox-override :class 'control :bind
  "add_theme_stylebox_override" :hash 4188838905)
 :void (name string-name) (stylebox style-box))

(defgmethod
 (control+add-theme-font-override :class 'control :bind
  "add_theme_font_override" :hash 3518018674)
 :void (name string-name) (font font))

(defgmethod
 (control+add-theme-font-size-override :class 'control :bind
  "add_theme_font_size_override" :hash 2415702435)
 :void (name string-name) (font-size int))

(defgmethod
 (control+add-theme-color-override :class 'control :bind
  "add_theme_color_override" :hash 4260178595)
 :void (name string-name) (color color))

(defgmethod
 (control+add-theme-constant-override :class 'control :bind
  "add_theme_constant_override" :hash 2415702435)
 :void (name string-name) (constant int))

(defgmethod
 (control+remove-theme-icon-override :class 'control :bind
  "remove_theme_icon_override" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (control+remove-theme-stylebox-override :class 'control :bind
  "remove_theme_stylebox_override" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (control+remove-theme-font-override :class 'control :bind
  "remove_theme_font_override" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (control+remove-theme-font-size-override :class 'control :bind
  "remove_theme_font_size_override" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (control+remove-theme-color-override :class 'control :bind
  "remove_theme_color_override" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (control+remove-theme-constant-override :class 'control :bind
  "remove_theme_constant_override" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (control+get-theme-icon :class 'control :bind "get_theme_icon" :hash
  3163973443)
 texture-2d (name string-name) (theme-type string-name))

(defgmethod
 (control+get-theme-stylebox :class 'control :bind "get_theme_stylebox" :hash
  604739069)
 style-box (name string-name) (theme-type string-name))

(defgmethod
 (control+get-theme-font :class 'control :bind "get_theme_font" :hash
  2826986490)
 font (name string-name) (theme-type string-name))

(defgmethod
 (control+get-theme-font-size :class 'control :bind "get_theme_font_size" :hash
  1327056374)
 int (name string-name) (theme-type string-name))

(defgmethod
 (control+get-theme-color :class 'control :bind "get_theme_color" :hash
  2798751242)
 color (name string-name) (theme-type string-name))

(defgmethod
 (control+get-theme-constant :class 'control :bind "get_theme_constant" :hash
  1327056374)
 int (name string-name) (theme-type string-name))

(defgmethod
 (control+has-theme-icon-override :class 'control :bind
  "has_theme_icon_override" :hash 2619796661)
 bool (name string-name))

(defgmethod
 (control+has-theme-stylebox-override :class 'control :bind
  "has_theme_stylebox_override" :hash 2619796661)
 bool (name string-name))

(defgmethod
 (control+has-theme-font-override :class 'control :bind
  "has_theme_font_override" :hash 2619796661)
 bool (name string-name))

(defgmethod
 (control+has-theme-font-size-override :class 'control :bind
  "has_theme_font_size_override" :hash 2619796661)
 bool (name string-name))

(defgmethod
 (control+has-theme-color-override :class 'control :bind
  "has_theme_color_override" :hash 2619796661)
 bool (name string-name))

(defgmethod
 (control+has-theme-constant-override :class 'control :bind
  "has_theme_constant_override" :hash 2619796661)
 bool (name string-name))

(defgmethod
 (control+has-theme-icon :class 'control :bind "has_theme_icon" :hash
  866386512)
 bool (name string-name) (theme-type string-name))

(defgmethod
 (control+has-theme-stylebox :class 'control :bind "has_theme_stylebox" :hash
  866386512)
 bool (name string-name) (theme-type string-name))

(defgmethod
 (control+has-theme-font :class 'control :bind "has_theme_font" :hash
  866386512)
 bool (name string-name) (theme-type string-name))

(defgmethod
 (control+has-theme-font-size :class 'control :bind "has_theme_font_size" :hash
  866386512)
 bool (name string-name) (theme-type string-name))

(defgmethod
 (control+has-theme-color :class 'control :bind "has_theme_color" :hash
  866386512)
 bool (name string-name) (theme-type string-name))

(defgmethod
 (control+has-theme-constant :class 'control :bind "has_theme_constant" :hash
  866386512)
 bool (name string-name) (theme-type string-name))

(defgmethod
 (control+get-theme-default-base-scale :class 'control :bind
  "get_theme_default_base_scale" :hash 1740695150)
 float)

(defgmethod
 (control+get-theme-default-font :class 'control :bind "get_theme_default_font"
  :hash 3229501585)
 font)

(defgmethod
 (control+get-theme-default-font-size :class 'control :bind
  "get_theme_default_font_size" :hash 3905245786)
 int)

(defgmethod
 (control+get-parent-control :class 'control :bind "get_parent_control" :hash
  2783021301)
 control)

(defgmethod
 (control+set-h-grow-direction :class 'control :bind "set_h_grow_direction"
  :hash 2022385301)
 :void (direction control+grow-direction))

(defgmethod
 (control+get-h-grow-direction :class 'control :bind "get_h_grow_direction"
  :hash 3635610155)
 control+grow-direction)

(defgmethod
 (control+set-v-grow-direction :class 'control :bind "set_v_grow_direction"
  :hash 2022385301)
 :void (direction control+grow-direction))

(defgmethod
 (control+get-v-grow-direction :class 'control :bind "get_v_grow_direction"
  :hash 3635610155)
 control+grow-direction)

(defgmethod
 (control+set-tooltip-auto-translate-mode :class 'control :bind
  "set_tooltip_auto_translate_mode" :hash 776149714)
 :void (mode node+auto-translate-mode))

(defgmethod
 (control+get-tooltip-auto-translate-mode :class 'control :bind
  "get_tooltip_auto_translate_mode" :hash 2498906432)
 node+auto-translate-mode)

(defgmethod
 (control+set-tooltip-text :class 'control :bind "set_tooltip_text" :hash
  83702148)
 :void (hint string))

(defgmethod
 (control+get-tooltip-text :class 'control :bind "get_tooltip_text" :hash
  201670096)
 string)

(defgmethod
 (control+get-tooltip :class 'control :bind "get_tooltip" :hash 2895288280)
 string (at-position vector-2))

(defgmethod
 (control+set-default-cursor-shape :class 'control :bind
  "set_default_cursor_shape" :hash 217062046)
 :void (shape control+cursor-shape))

(defgmethod
 (control+get-default-cursor-shape :class 'control :bind
  "get_default_cursor_shape" :hash 2359535750)
 control+cursor-shape)

(defgmethod
 (control+get-cursor-shape :class 'control :bind "get_cursor_shape" :hash
  1395773853)
 control+cursor-shape (position vector-2))

(defgmethod
 (control+set-focus-neighbor :class 'control :bind "set_focus_neighbor" :hash
  2024461774)
 :void (side side) (neighbor node-path))

(defgmethod
 (control+get-focus-neighbor :class 'control :bind "get_focus_neighbor" :hash
  2757935761)
 node-path (side side))

(defgmethod
 (control+set-focus-next :class 'control :bind "set_focus_next" :hash
  1348162250)
 :void (next node-path))

(defgmethod
 (control+get-focus-next :class 'control :bind "get_focus_next" :hash
  4075236667)
 node-path)

(defgmethod
 (control+set-focus-previous :class 'control :bind "set_focus_previous" :hash
  1348162250)
 :void (previous node-path))

(defgmethod
 (control+get-focus-previous :class 'control :bind "get_focus_previous" :hash
  4075236667)
 node-path)

(defgmethod
 (control+force-drag :class 'control :bind "force_drag" :hash 3191844692) :void
 (data variant) (preview control))

(defgmethod
 (control+accessibility-drag :class 'control :bind "accessibility_drag" :hash
  3218959716)
 :void)

(defgmethod
 (control+accessibility-drop :class 'control :bind "accessibility_drop" :hash
  3218959716)
 :void)

(defgmethod
 (control+set-accessibility-name :class 'control :bind "set_accessibility_name"
  :hash 83702148)
 :void (name string))

(defgmethod
 (control+get-accessibility-name :class 'control :bind "get_accessibility_name"
  :hash 201670096)
 string)

(defgmethod
 (control+set-accessibility-description :class 'control :bind
  "set_accessibility_description" :hash 83702148)
 :void (description string))

(defgmethod
 (control+get-accessibility-description :class 'control :bind
  "get_accessibility_description" :hash 201670096)
 string)

(defgmethod
 (control+set-accessibility-live :class 'control :bind "set_accessibility_live"
  :hash 1720261470)
 :void (mode display-server+accessibility-live-mode))

(defgmethod
 (control+get-accessibility-live :class 'control :bind "get_accessibility_live"
  :hash 3311037003)
 display-server+accessibility-live-mode)

(defgmethod
 (control+set-accessibility-controls-nodes :class 'control :bind
  "set_accessibility_controls_nodes" :hash 381264803)
 :void (node-path array))

(defgmethod
 (control+get-accessibility-controls-nodes :class 'control :bind
  "get_accessibility_controls_nodes" :hash 3995934104)
 array)

(defgmethod
 (control+set-accessibility-described-by-nodes :class 'control :bind
  "set_accessibility_described_by_nodes" :hash 381264803)
 :void (node-path array))

(defgmethod
 (control+get-accessibility-described-by-nodes :class 'control :bind
  "get_accessibility_described_by_nodes" :hash 3995934104)
 array)

(defgmethod
 (control+set-accessibility-labeled-by-nodes :class 'control :bind
  "set_accessibility_labeled_by_nodes" :hash 381264803)
 :void (node-path array))

(defgmethod
 (control+get-accessibility-labeled-by-nodes :class 'control :bind
  "get_accessibility_labeled_by_nodes" :hash 3995934104)
 array)

(defgmethod
 (control+set-accessibility-flow-to-nodes :class 'control :bind
  "set_accessibility_flow_to_nodes" :hash 381264803)
 :void (node-path array))

(defgmethod
 (control+get-accessibility-flow-to-nodes :class 'control :bind
  "get_accessibility_flow_to_nodes" :hash 3995934104)
 array)

(defgmethod
 (control+set-mouse-filter :class 'control :bind "set_mouse_filter" :hash
  3891156122)
 :void (filter control+mouse-filter))

(defgmethod
 (control+get-mouse-filter :class 'control :bind "get_mouse_filter" :hash
  1572545674)
 control+mouse-filter)

(defgmethod
 (control+get-mouse-filter-with-override :class 'control :bind
  "get_mouse_filter_with_override" :hash 1572545674)
 control+mouse-filter)

(defgmethod
 (control+set-mouse-behavior-recursive :class 'control :bind
  "set_mouse_behavior_recursive" :hash 849284636)
 :void (mouse-behavior-recursive control+mouse-behavior-recursive))

(defgmethod
 (control+get-mouse-behavior-recursive :class 'control :bind
  "get_mouse_behavior_recursive" :hash 3779367402)
 control+mouse-behavior-recursive)

(defgmethod
 (control+set-force-pass-scroll-events :class 'control :bind
  "set_force_pass_scroll_events" :hash 2586408642)
 :void (force-pass-scroll-events bool))

(defgmethod
 (control+is-force-pass-scroll-events :class 'control :bind
  "is_force_pass_scroll_events" :hash 36873697)
 bool)

(defgmethod
 (control+set-clip-contents :class 'control :bind "set_clip_contents" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (control+is-clipping-contents :class 'control :bind "is_clipping_contents"
  :hash 2240911060)
 bool)

(defgmethod
 (control+grab-click-focus :class 'control :bind "grab_click_focus" :hash
  3218959716)
 :void)

(defgmethod
 (control+set-drag-forwarding :class 'control :bind "set_drag_forwarding" :hash
  1076571380)
 :void (drag-func callable) (can-drop-func callable) (drop-func callable))

(defgmethod
 (control+set-drag-preview :class 'control :bind "set_drag_preview" :hash
  1496901182)
 :void (control control))

(defgmethod
 (control+is-drag-successful :class 'control :bind "is_drag_successful" :hash
  36873697)
 bool)

(defgmethod
 (control+warp-mouse :class 'control :bind "warp_mouse" :hash 743155724) :void
 (position vector-2))

(defgmethod
 (control+set-shortcut-context :class 'control :bind "set_shortcut_context"
  :hash 1078189570)
 :void (node node))

(defgmethod
 (control+get-shortcut-context :class 'control :bind "get_shortcut_context"
  :hash 3160264692)
 node)

(defgmethod
 (control+update-minimum-size :class 'control :bind "update_minimum_size" :hash
  3218959716)
 :void)

(defgmethod
 (control+set-layout-direction :class 'control :bind "set_layout_direction"
  :hash 3310692370)
 :void (direction control+layout-direction))

(defgmethod
 (control+get-layout-direction :class 'control :bind "get_layout_direction"
  :hash 1546772008)
 control+layout-direction)

(defgmethod
 (control+is-layout-rtl :class 'control :bind "is_layout_rtl" :hash 36873697)
 bool)

(defgmethod
 (control+set-auto-translate :class 'control :bind "set_auto_translate" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (control+is-auto-translating :class 'control :bind "is_auto_translating" :hash
  36873697)
 bool)

(defgmethod
 (control+set-localize-numeral-system :class 'control :bind
  "set_localize_numeral_system" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (control+is-localizing-numeral-system :class 'control :bind
  "is_localizing_numeral_system" :hash 36873697)
 bool)