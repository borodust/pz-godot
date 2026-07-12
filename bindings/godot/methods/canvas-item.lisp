(common-lisp:in-package :%godot)


(defgmethod
 (canvas-item+%draw :class 'canvas-item :bind "_draw" :hash 3218959716 :virtual
  common-lisp:t)
 :void)

(defgmethod
 (canvas-item+get-canvas-item :class 'canvas-item :bind "get_canvas_item" :hash
  2944877500)
 rid)

(defgmethod
 (canvas-item+set-visible :class 'canvas-item :bind "set_visible" :hash
  2586408642)
 :void (visible bool))

(defgmethod
 (canvas-item+is-visible :class 'canvas-item :bind "is_visible" :hash 36873697)
 bool)

(defgmethod
 (canvas-item+is-visible-in-tree :class 'canvas-item :bind "is_visible_in_tree"
  :hash 36873697)
 bool)

(defgmethod
 (canvas-item+show :class 'canvas-item :bind "show" :hash 3218959716) :void)

(defgmethod
 (canvas-item+hide :class 'canvas-item :bind "hide" :hash 3218959716) :void)

(defgmethod
 (canvas-item+queue-redraw :class 'canvas-item :bind "queue_redraw" :hash
  3218959716)
 :void)

(defgmethod
 (canvas-item+move-to-front :class 'canvas-item :bind "move_to_front" :hash
  3218959716)
 :void)

(defgmethod
 (canvas-item+set-as-top-level :class 'canvas-item :bind "set_as_top_level"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (canvas-item+is-set-as-top-level :class 'canvas-item :bind
  "is_set_as_top_level" :hash 36873697)
 bool)

(defgmethod
 (canvas-item+set-light-mask :class 'canvas-item :bind "set_light_mask" :hash
  1286410249)
 :void (light-mask int))

(defgmethod
 (canvas-item+get-light-mask :class 'canvas-item :bind "get_light_mask" :hash
  3905245786)
 int)

(defgmethod
 (canvas-item+set-modulate :class 'canvas-item :bind "set_modulate" :hash
  2920490490)
 :void (modulate color))

(defgmethod
 (canvas-item+get-modulate :class 'canvas-item :bind "get_modulate" :hash
  3444240500)
 color)

(defgmethod
 (canvas-item+set-self-modulate :class 'canvas-item :bind "set_self_modulate"
  :hash 2920490490)
 :void (self-modulate color))

(defgmethod
 (canvas-item+get-self-modulate :class 'canvas-item :bind "get_self_modulate"
  :hash 3444240500)
 color)

(defgmethod
 (canvas-item+set-z-index :class 'canvas-item :bind "set_z_index" :hash
  1286410249)
 :void (z-index int))

(defgmethod
 (canvas-item+get-z-index :class 'canvas-item :bind "get_z_index" :hash
  3905245786)
 int)

(defgmethod
 (canvas-item+set-z-as-relative :class 'canvas-item :bind "set_z_as_relative"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (canvas-item+is-z-relative :class 'canvas-item :bind "is_z_relative" :hash
  36873697)
 bool)

(defgmethod
 (canvas-item+set-y-sort-enabled :class 'canvas-item :bind "set_y_sort_enabled"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (canvas-item+is-y-sort-enabled :class 'canvas-item :bind "is_y_sort_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (canvas-item+set-draw-behind-parent :class 'canvas-item :bind
  "set_draw_behind_parent" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (canvas-item+is-draw-behind-parent-enabled :class 'canvas-item :bind
  "is_draw_behind_parent_enabled" :hash 36873697)
 bool)

(defgmethod
 (canvas-item+draw-line :class 'canvas-item :bind "draw_line" :hash 1562330099)
 :void (from vector-2) (to vector-2) (color color) (width float)
 (antialiased bool))

(defgmethod
 (canvas-item+draw-dashed-line :class 'canvas-item :bind "draw_dashed_line"
  :hash 3653831622)
 :void (from vector-2) (to vector-2) (color color) (width float) (dash float)
 (aligned bool) (antialiased bool))

(defgmethod
 (canvas-item+draw-polyline :class 'canvas-item :bind "draw_polyline" :hash
  3797364428)
 :void (points packed-vector-2array) (color color) (width float)
 (antialiased bool))

(defgmethod
 (canvas-item+draw-polyline-colors :class 'canvas-item :bind
  "draw_polyline_colors" :hash 2311979562)
 :void (points packed-vector-2array) (colors packed-color-array) (width float)
 (antialiased bool))

(defgmethod
 (canvas-item+draw-ellipse-arc :class 'canvas-item :bind "draw_ellipse_arc"
  :hash 936174114)
 :void (center vector-2) (major float) (minor float) (start-angle float)
 (end-angle float) (point-count int) (color color) (width float)
 (antialiased bool))

(defgmethod
 (canvas-item+draw-arc :class 'canvas-item :bind "draw_arc" :hash 4140652635)
 :void (center vector-2) (radius float) (start-angle float) (end-angle float)
 (point-count int) (color color) (width float) (antialiased bool))

(defgmethod
 (canvas-item+draw-multiline :class 'canvas-item :bind "draw_multiline" :hash
  3797364428)
 :void (points packed-vector-2array) (color color) (width float)
 (antialiased bool))

(defgmethod
 (canvas-item+draw-multiline-colors :class 'canvas-item :bind
  "draw_multiline_colors" :hash 2311979562)
 :void (points packed-vector-2array) (colors packed-color-array) (width float)
 (antialiased bool))

(defgmethod
 (canvas-item+draw-rect :class 'canvas-item :bind "draw_rect" :hash 2773573813)
 :void (rect rect-2) (color color) (filled bool) (width float)
 (antialiased bool))

(defgmethod
 (canvas-item+draw-circle :class 'canvas-item :bind "draw_circle" :hash
  3153026596)
 :void (position vector-2) (radius float) (color color) (filled bool)
 (width float) (antialiased bool))

(defgmethod
 (canvas-item+draw-ellipse :class 'canvas-item :bind "draw_ellipse" :hash
  3790774806)
 :void (position vector-2) (major float) (minor float) (color color)
 (filled bool) (width float) (antialiased bool))

(defgmethod
 (canvas-item+draw-texture :class 'canvas-item :bind "draw_texture" :hash
  520200117)
 :void (texture texture-2d) (position vector-2) (modulate color))

(defgmethod
 (canvas-item+draw-texture-rect :class 'canvas-item :bind "draw_texture_rect"
  :hash 3832805018)
 :void (texture texture-2d) (rect rect-2) (tile bool) (modulate color)
 (transpose bool))

(defgmethod
 (canvas-item+draw-texture-rect-region :class 'canvas-item :bind
  "draw_texture_rect_region" :hash 3883821411)
 :void (texture texture-2d) (rect rect-2) (src-rect rect-2) (modulate color)
 (transpose bool) (clip-uv bool))

(defgmethod
 (canvas-item+draw-msdf-texture-rect-region :class 'canvas-item :bind
  "draw_msdf_texture_rect_region" :hash 4219163252)
 :void (texture texture-2d) (rect rect-2) (src-rect rect-2) (modulate color)
 (outline float) (pixel-range float) (scale float))

(defgmethod
 (canvas-item+draw-lcd-texture-rect-region :class 'canvas-item :bind
  "draw_lcd_texture_rect_region" :hash 3212350954)
 :void (texture texture-2d) (rect rect-2) (src-rect rect-2) (modulate color))

(defgmethod
 (canvas-item+draw-style-box :class 'canvas-item :bind "draw_style_box" :hash
  388176283)
 :void (style-box style-box) (rect rect-2))

(defgmethod
 (canvas-item+draw-primitive :class 'canvas-item :bind "draw_primitive" :hash
  3288481815)
 :void (points packed-vector-2array) (colors packed-color-array)
 (uvs packed-vector-2array) (texture texture-2d))

(defgmethod
 (canvas-item+draw-polygon :class 'canvas-item :bind "draw_polygon" :hash
  974537912)
 :void (points packed-vector-2array) (colors packed-color-array)
 (uvs packed-vector-2array) (texture texture-2d))

(defgmethod
 (canvas-item+draw-colored-polygon :class 'canvas-item :bind
  "draw_colored_polygon" :hash 15245644)
 :void (points packed-vector-2array) (color color) (uvs packed-vector-2array)
 (texture texture-2d))

(defgmethod
 (canvas-item+draw-string :class 'canvas-item :bind "draw_string" :hash
  719605945)
 :void (font font) (pos vector-2) (text string)
 (alignment horizontal-alignment) (width float) (font-size int)
 (modulate color) (justification-flags text-server+justification-flag)
 (direction text-server+direction) (orientation text-server+orientation)
 (oversampling float))

(defgmethod
 (canvas-item+draw-multiline-string :class 'canvas-item :bind
  "draw_multiline_string" :hash 2341488182)
 :void (font font) (pos vector-2) (text string)
 (alignment horizontal-alignment) (width float) (font-size int) (max-lines int)
 (modulate color) (brk-flags text-server+line-break-flag)
 (justification-flags text-server+justification-flag)
 (direction text-server+direction) (orientation text-server+orientation)
 (oversampling float))

(defgmethod
 (canvas-item+draw-string-outline :class 'canvas-item :bind
  "draw_string_outline" :hash 707403449)
 :void (font font) (pos vector-2) (text string)
 (alignment horizontal-alignment) (width float) (font-size int) (size int)
 (modulate color) (justification-flags text-server+justification-flag)
 (direction text-server+direction) (orientation text-server+orientation)
 (oversampling float))

(defgmethod
 (canvas-item+draw-multiline-string-outline :class 'canvas-item :bind
  "draw_multiline_string_outline" :hash 3050414441)
 :void (font font) (pos vector-2) (text string)
 (alignment horizontal-alignment) (width float) (font-size int) (max-lines int)
 (size int) (modulate color) (brk-flags text-server+line-break-flag)
 (justification-flags text-server+justification-flag)
 (direction text-server+direction) (orientation text-server+orientation)
 (oversampling float))

(defgmethod
 (canvas-item+draw-char :class 'canvas-item :bind "draw_char" :hash 1336210142)
 :void (font font) (pos vector-2) (char string) (font-size int)
 (modulate color) (oversampling float))

(defgmethod
 (canvas-item+draw-char-outline :class 'canvas-item :bind "draw_char_outline"
  :hash 1846384149)
 :void (font font) (pos vector-2) (char string) (font-size int) (size int)
 (modulate color) (oversampling float))

(defgmethod
 (canvas-item+draw-mesh :class 'canvas-item :bind "draw_mesh" :hash 153818295)
 :void (mesh mesh) (texture texture-2d) (transform transform-2d)
 (modulate color))

(defgmethod
 (canvas-item+draw-multimesh :class 'canvas-item :bind "draw_multimesh" :hash
  937992368)
 :void (multimesh multi-mesh) (texture texture-2d))

(defgmethod
 (canvas-item+draw-set-transform :class 'canvas-item :bind "draw_set_transform"
  :hash 288975085)
 :void (position vector-2) (rotation float) (scale vector-2))

(defgmethod
 (canvas-item+draw-set-transform-matrix :class 'canvas-item :bind
  "draw_set_transform_matrix" :hash 2761652528)
 :void (xform transform-2d))

(defgmethod
 (canvas-item+draw-animation-slice :class 'canvas-item :bind
  "draw_animation_slice" :hash 3112831842)
 :void (animation-length float) (slice-begin float) (slice-end float)
 (offset float))

(defgmethod
 (canvas-item+draw-end-animation :class 'canvas-item :bind "draw_end_animation"
  :hash 3218959716)
 :void)

(defgmethod
 (canvas-item+get-transform :class 'canvas-item :bind "get_transform" :hash
  3814499831)
 transform-2d)

(defgmethod
 (canvas-item+get-global-transform :class 'canvas-item :bind
  "get_global_transform" :hash 3814499831)
 transform-2d)

(defgmethod
 (canvas-item+get-global-transform-with-canvas :class 'canvas-item :bind
  "get_global_transform_with_canvas" :hash 3814499831)
 transform-2d)

(defgmethod
 (canvas-item+get-viewport-transform :class 'canvas-item :bind
  "get_viewport_transform" :hash 3814499831)
 transform-2d)

(defgmethod
 (canvas-item+get-viewport-rect :class 'canvas-item :bind "get_viewport_rect"
  :hash 1639390495)
 rect-2)

(defgmethod
 (canvas-item+get-canvas-transform :class 'canvas-item :bind
  "get_canvas_transform" :hash 3814499831)
 transform-2d)

(defgmethod
 (canvas-item+get-screen-transform :class 'canvas-item :bind
  "get_screen_transform" :hash 3814499831)
 transform-2d)

(defgmethod
 (canvas-item+get-local-mouse-position :class 'canvas-item :bind
  "get_local_mouse_position" :hash 3341600327)
 vector-2)

(defgmethod
 (canvas-item+get-global-mouse-position :class 'canvas-item :bind
  "get_global_mouse_position" :hash 3341600327)
 vector-2)

(defgmethod
 (canvas-item+get-canvas :class 'canvas-item :bind "get_canvas" :hash
  2944877500)
 rid)

(defgmethod
 (canvas-item+get-canvas-layer-node :class 'canvas-item :bind
  "get_canvas_layer_node" :hash 2602762519)
 canvas-layer)

(defgmethod
 (canvas-item+get-world-2d :class 'canvas-item :bind "get_world_2d" :hash
  2339128592)
 world-2d)

(defgmethod
 (canvas-item+set-material :class 'canvas-item :bind "set_material" :hash
  2757459619)
 :void (material material))

(defgmethod
 (canvas-item+get-material :class 'canvas-item :bind "get_material" :hash
  5934680)
 material)

(defgmethod
 (canvas-item+set-instance-shader-parameter :class 'canvas-item :bind
  "set_instance_shader_parameter" :hash 3776071444)
 :void (name string-name) (value variant))

(defgmethod
 (canvas-item+get-instance-shader-parameter :class 'canvas-item :bind
  "get_instance_shader_parameter" :hash 2760726917)
 variant (name string-name))

(defgmethod
 (canvas-item+set-use-parent-material :class 'canvas-item :bind
  "set_use_parent_material" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (canvas-item+get-use-parent-material :class 'canvas-item :bind
  "get_use_parent_material" :hash 36873697)
 bool)

(defgmethod
 (canvas-item+set-notify-local-transform :class 'canvas-item :bind
  "set_notify_local_transform" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (canvas-item+is-local-transform-notification-enabled :class 'canvas-item :bind
  "is_local_transform_notification_enabled" :hash 36873697)
 bool)

(defgmethod
 (canvas-item+set-notify-transform :class 'canvas-item :bind
  "set_notify_transform" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (canvas-item+is-transform-notification-enabled :class 'canvas-item :bind
  "is_transform_notification_enabled" :hash 36873697)
 bool)

(defgmethod
 (canvas-item+force-update-transform :class 'canvas-item :bind
  "force_update_transform" :hash 3218959716)
 :void)

(defgmethod
 (canvas-item+make-canvas-position-local :class 'canvas-item :bind
  "make_canvas_position_local" :hash 2656412154)
 vector-2 (viewport-point vector-2))

(defgmethod
 (canvas-item+make-input-local :class 'canvas-item :bind "make_input_local"
  :hash 811130057)
 input-event (event input-event))

(defgmethod
 (canvas-item+set-visibility-layer :class 'canvas-item :bind
  "set_visibility_layer" :hash 1286410249)
 :void (layer int))

(defgmethod
 (canvas-item+get-visibility-layer :class 'canvas-item :bind
  "get_visibility_layer" :hash 3905245786)
 int)

(defgmethod
 (canvas-item+set-visibility-layer-bit :class 'canvas-item :bind
  "set_visibility_layer_bit" :hash 300928843)
 :void (layer int) (enabled bool))

(defgmethod
 (canvas-item+get-visibility-layer-bit :class 'canvas-item :bind
  "get_visibility_layer_bit" :hash 1116898809)
 bool (layer int))

(defgmethod
 (canvas-item+set-texture-filter :class 'canvas-item :bind "set_texture_filter"
  :hash 1037999706)
 :void (mode canvas-item+texture-filter))

(defgmethod
 (canvas-item+get-texture-filter :class 'canvas-item :bind "get_texture_filter"
  :hash 121960042)
 canvas-item+texture-filter)

(defgmethod
 (canvas-item+set-texture-repeat :class 'canvas-item :bind "set_texture_repeat"
  :hash 1716472974)
 :void (mode canvas-item+texture-repeat))

(defgmethod
 (canvas-item+get-texture-repeat :class 'canvas-item :bind "get_texture_repeat"
  :hash 2667158319)
 canvas-item+texture-repeat)

(defgmethod
 (canvas-item+set-clip-children-mode :class 'canvas-item :bind
  "set_clip_children_mode" :hash 1319393776)
 :void (mode canvas-item+clip-children-mode))

(defgmethod
 (canvas-item+get-clip-children-mode :class 'canvas-item :bind
  "get_clip_children_mode" :hash 3581808349)
 canvas-item+clip-children-mode)

(defgmethod
 (canvas-item+set-oversampling-with-scale :class 'canvas-item :bind
  "set_oversampling_with_scale" :hash 872218804)
 :void (enabled canvas-item+oversampling-with-scale))

(defgmethod
 (canvas-item+get-oversampling-with-scale :class 'canvas-item :bind
  "get_oversampling_with_scale" :hash 2026097197)
 canvas-item+oversampling-with-scale)