(common-lisp:in-package :%godot)


(defgmethod
 (window+-get-contents-minimum-size :class 'window :bind
  "_get_contents_minimum_size" :hash 3341600327 :virtual common-lisp:t)
 vector-2)

(defgmethod (window+set-title :class 'window :bind "set_title" :hash 83702148)
 :void (title string))

(defgmethod (window+get-title :class 'window :bind "get_title" :hash 201670096)
 string)

(defgmethod
 (window+set-initial-position :class 'window :bind "set_initial_position" :hash
  4084468099)
 :void (initial-position window+window-initial-position))

(defgmethod
 (window+get-initial-position :class 'window :bind "get_initial_position" :hash
  4294066647)
 window+window-initial-position)

(defgmethod
 (window+set-current-screen :class 'window :bind "set_current_screen" :hash
  1286410249)
 :void (index int))

(defgmethod
 (window+get-current-screen :class 'window :bind "get_current_screen" :hash
  3905245786)
 int)

(defgmethod
 (window+set-position :class 'window :bind "set_position" :hash 1130785943)
 :void (position vector-2i))

(defgmethod
 (window+get-position :class 'window :bind "get_position" :hash 3690982128)
 vector-2i)

(defgmethod
 (window+move-to-center :class 'window :bind "move_to_center" :hash 3218959716)
 :void)

(defgmethod (window+set-size :class 'window :bind "set_size" :hash 1130785943)
 :void (size vector-2i))

(defgmethod (window+get-size :class 'window :bind "get_size" :hash 3690982128)
 vector-2i)

(defgmethod
 (window+reset-size :class 'window :bind "reset_size" :hash 3218959716) :void)

(defgmethod
 (window+get-position-with-decorations :class 'window :bind
  "get_position_with_decorations" :hash 3690982128)
 vector-2i)

(defgmethod
 (window+get-size-with-decorations :class 'window :bind
  "get_size_with_decorations" :hash 3690982128)
 vector-2i)

(defgmethod
 (window+set-max-size :class 'window :bind "set_max_size" :hash 1130785943)
 :void (max-size vector-2i))

(defgmethod
 (window+get-max-size :class 'window :bind "get_max_size" :hash 3690982128)
 vector-2i)

(defgmethod
 (window+set-min-size :class 'window :bind "set_min_size" :hash 1130785943)
 :void (min-size vector-2i))

(defgmethod
 (window+get-min-size :class 'window :bind "get_min_size" :hash 3690982128)
 vector-2i)

(defgmethod (window+set-mode :class 'window :bind "set_mode" :hash 3095236531)
 :void (mode window+mode))

(defgmethod (window+get-mode :class 'window :bind "get_mode" :hash 2566346114)
 window+mode)

(defgmethod (window+set-flag :class 'window :bind "set_flag" :hash 3426449779)
 :void (flag window+flags) (enabled bool))

(defgmethod (window+get-flag :class 'window :bind "get_flag" :hash 3062752289)
 bool (flag window+flags))

(defgmethod
 (window+is-maximize-allowed :class 'window :bind "is_maximize_allowed" :hash
  36873697)
 bool)

(defgmethod
 (window+request-attention :class 'window :bind "request_attention" :hash
  3218959716)
 :void)

(defgmethod
 (window+move-to-foreground :class 'window :bind "move_to_foreground" :hash
  3218959716)
 :void)

(defgmethod
 (window+set-visible :class 'window :bind "set_visible" :hash 2586408642) :void
 (visible bool))

(defgmethod
 (window+is-visible :class 'window :bind "is_visible" :hash 36873697) bool)

(defgmethod (window+hide :class 'window :bind "hide" :hash 3218959716) :void)

(defgmethod (window+show :class 'window :bind "show" :hash 3218959716) :void)

(defgmethod
 (window+set-transient :class 'window :bind "set_transient" :hash 2586408642)
 :void (transient bool))

(defgmethod
 (window+is-transient :class 'window :bind "is_transient" :hash 36873697) bool)

(defgmethod
 (window+set-transient-to-focused :class 'window :bind
  "set_transient_to_focused" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (window+is-transient-to-focused :class 'window :bind "is_transient_to_focused"
  :hash 36873697)
 bool)

(defgmethod
 (window+set-exclusive :class 'window :bind "set_exclusive" :hash 2586408642)
 :void (exclusive bool))

(defgmethod
 (window+is-exclusive :class 'window :bind "is_exclusive" :hash 36873697) bool)

(defgmethod
 (window+set-unparent-when-invisible :class 'window :bind
  "set_unparent_when_invisible" :hash 2586408642)
 :void (unparent bool))

(defgmethod (window+can-draw :class 'window :bind "can_draw" :hash 36873697)
 bool)

(defgmethod (window+has-focus :class 'window :bind "has_focus" :hash 36873697)
 bool)

(defgmethod
 (window+grab-focus :class 'window :bind "grab_focus" :hash 3218959716) :void)

(defgmethod
 (window+start-drag :class 'window :bind "start_drag" :hash 3218959716) :void)

(defgmethod
 (window+start-resize :class 'window :bind "start_resize" :hash 122288853)
 :void (edge display-server+window-resize-edge))

(defgmethod
 (window+set-ime-active :class 'window :bind "set_ime_active" :hash 2586408642)
 :void (active bool))

(defgmethod
 (window+set-ime-position :class 'window :bind "set_ime_position" :hash
  1130785943)
 :void (position vector-2i))

(defgmethod
 (window+is-embedded :class 'window :bind "is_embedded" :hash 36873697) bool)

(defgmethod
 (window+get-contents-minimum-size :class 'window :bind
  "get_contents_minimum_size" :hash 3341600327)
 vector-2)

(defgmethod
 (window+set-force-native :class 'window :bind "set_force_native" :hash
  2586408642)
 :void (force-native bool))

(defgmethod
 (window+get-force-native :class 'window :bind "get_force_native" :hash
  36873697)
 bool)

(defgmethod
 (window+set-content-scale-size :class 'window :bind "set_content_scale_size"
  :hash 1130785943)
 :void (size vector-2i))

(defgmethod
 (window+get-content-scale-size :class 'window :bind "get_content_scale_size"
  :hash 3690982128)
 vector-2i)

(defgmethod
 (window+set-content-scale-mode :class 'window :bind "set_content_scale_mode"
  :hash 2937716473)
 :void (mode window+content-scale-mode))

(defgmethod
 (window+get-content-scale-mode :class 'window :bind "get_content_scale_mode"
  :hash 161585230)
 window+content-scale-mode)

(defgmethod
 (window+set-content-scale-aspect :class 'window :bind
  "set_content_scale_aspect" :hash 2370399418)
 :void (aspect window+content-scale-aspect))

(defgmethod
 (window+get-content-scale-aspect :class 'window :bind
  "get_content_scale_aspect" :hash 4158790715)
 window+content-scale-aspect)

(defgmethod
 (window+set-content-scale-stretch :class 'window :bind
  "set_content_scale_stretch" :hash 349355940)
 :void (stretch window+content-scale-stretch))

(defgmethod
 (window+get-content-scale-stretch :class 'window :bind
  "get_content_scale_stretch" :hash 536857316)
 window+content-scale-stretch)

(defgmethod
 (window+set-nonclient-area :class 'window :bind "set_nonclient_area" :hash
  1763793166)
 :void (area rect-2i))

(defgmethod
 (window+get-nonclient-area :class 'window :bind "get_nonclient_area" :hash
  410525958)
 rect-2i)

(defgmethod
 (window+set-keep-title-visible :class 'window :bind "set_keep_title_visible"
  :hash 2586408642)
 :void (title-visible bool))

(defgmethod
 (window+get-keep-title-visible :class 'window :bind "get_keep_title_visible"
  :hash 36873697)
 bool)

(defgmethod
 (window+set-content-scale-factor :class 'window :bind
  "set_content_scale_factor" :hash 373806689)
 :void (factor float))

(defgmethod
 (window+get-content-scale-factor :class 'window :bind
  "get_content_scale_factor" :hash 1740695150)
 float)

(defgmethod
 (window+set-mouse-passthrough-polygon :class 'window :bind
  "set_mouse_passthrough_polygon" :hash 1509147220)
 :void (polygon packed-vector-2array))

(defgmethod
 (window+get-mouse-passthrough-polygon :class 'window :bind
  "get_mouse_passthrough_polygon" :hash 2961356807)
 packed-vector-2array)

(defgmethod
 (window+set-wrap-controls :class 'window :bind "set_wrap_controls" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (window+is-wrapping-controls :class 'window :bind "is_wrapping_controls" :hash
  36873697)
 bool)

(defgmethod
 (window+child-controls-changed :class 'window :bind "child_controls_changed"
  :hash 3218959716)
 :void)

(defgmethod
 (window+set-theme :class 'window :bind "set_theme" :hash 2326690814) :void
 (theme theme))

(defgmethod
 (window+get-theme :class 'window :bind "get_theme" :hash 3846893731) theme)

(defgmethod
 (window+set-theme-type-variation :class 'window :bind
  "set_theme_type_variation" :hash 3304788590)
 :void (theme-type string-name))

(defgmethod
 (window+get-theme-type-variation :class 'window :bind
  "get_theme_type_variation" :hash 2002593661)
 string-name)

(defgmethod
 (window+begin-bulk-theme-override :class 'window :bind
  "begin_bulk_theme_override" :hash 3218959716)
 :void)

(defgmethod
 (window+end-bulk-theme-override :class 'window :bind "end_bulk_theme_override"
  :hash 3218959716)
 :void)

(defgmethod
 (window+add-theme-icon-override :class 'window :bind "add_theme_icon_override"
  :hash 1373065600)
 :void (name string-name) (texture texture-2d))

(defgmethod
 (window+add-theme-stylebox-override :class 'window :bind
  "add_theme_stylebox_override" :hash 4188838905)
 :void (name string-name) (stylebox style-box))

(defgmethod
 (window+add-theme-font-override :class 'window :bind "add_theme_font_override"
  :hash 3518018674)
 :void (name string-name) (font font))

(defgmethod
 (window+add-theme-font-size-override :class 'window :bind
  "add_theme_font_size_override" :hash 2415702435)
 :void (name string-name) (font-size int))

(defgmethod
 (window+add-theme-color-override :class 'window :bind
  "add_theme_color_override" :hash 4260178595)
 :void (name string-name) (color color))

(defgmethod
 (window+add-theme-constant-override :class 'window :bind
  "add_theme_constant_override" :hash 2415702435)
 :void (name string-name) (constant int))

(defgmethod
 (window+remove-theme-icon-override :class 'window :bind
  "remove_theme_icon_override" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (window+remove-theme-stylebox-override :class 'window :bind
  "remove_theme_stylebox_override" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (window+remove-theme-font-override :class 'window :bind
  "remove_theme_font_override" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (window+remove-theme-font-size-override :class 'window :bind
  "remove_theme_font_size_override" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (window+remove-theme-color-override :class 'window :bind
  "remove_theme_color_override" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (window+remove-theme-constant-override :class 'window :bind
  "remove_theme_constant_override" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (window+get-theme-icon :class 'window :bind "get_theme_icon" :hash 3163973443)
 texture-2d (name string-name) (theme-type string-name))

(defgmethod
 (window+get-theme-stylebox :class 'window :bind "get_theme_stylebox" :hash
  604739069)
 style-box (name string-name) (theme-type string-name))

(defgmethod
 (window+get-theme-font :class 'window :bind "get_theme_font" :hash 2826986490)
 font (name string-name) (theme-type string-name))

(defgmethod
 (window+get-theme-font-size :class 'window :bind "get_theme_font_size" :hash
  1327056374)
 int (name string-name) (theme-type string-name))

(defgmethod
 (window+get-theme-color :class 'window :bind "get_theme_color" :hash
  2798751242)
 color (name string-name) (theme-type string-name))

(defgmethod
 (window+get-theme-constant :class 'window :bind "get_theme_constant" :hash
  1327056374)
 int (name string-name) (theme-type string-name))

(defgmethod
 (window+has-theme-icon-override :class 'window :bind "has_theme_icon_override"
  :hash 2619796661)
 bool (name string-name))

(defgmethod
 (window+has-theme-stylebox-override :class 'window :bind
  "has_theme_stylebox_override" :hash 2619796661)
 bool (name string-name))

(defgmethod
 (window+has-theme-font-override :class 'window :bind "has_theme_font_override"
  :hash 2619796661)
 bool (name string-name))

(defgmethod
 (window+has-theme-font-size-override :class 'window :bind
  "has_theme_font_size_override" :hash 2619796661)
 bool (name string-name))

(defgmethod
 (window+has-theme-color-override :class 'window :bind
  "has_theme_color_override" :hash 2619796661)
 bool (name string-name))

(defgmethod
 (window+has-theme-constant-override :class 'window :bind
  "has_theme_constant_override" :hash 2619796661)
 bool (name string-name))

(defgmethod
 (window+has-theme-icon :class 'window :bind "has_theme_icon" :hash 866386512)
 bool (name string-name) (theme-type string-name))

(defgmethod
 (window+has-theme-stylebox :class 'window :bind "has_theme_stylebox" :hash
  866386512)
 bool (name string-name) (theme-type string-name))

(defgmethod
 (window+has-theme-font :class 'window :bind "has_theme_font" :hash 866386512)
 bool (name string-name) (theme-type string-name))

(defgmethod
 (window+has-theme-font-size :class 'window :bind "has_theme_font_size" :hash
  866386512)
 bool (name string-name) (theme-type string-name))

(defgmethod
 (window+has-theme-color :class 'window :bind "has_theme_color" :hash
  866386512)
 bool (name string-name) (theme-type string-name))

(defgmethod
 (window+has-theme-constant :class 'window :bind "has_theme_constant" :hash
  866386512)
 bool (name string-name) (theme-type string-name))

(defgmethod
 (window+get-theme-default-base-scale :class 'window :bind
  "get_theme_default_base_scale" :hash 1740695150)
 float)

(defgmethod
 (window+get-theme-default-font :class 'window :bind "get_theme_default_font"
  :hash 3229501585)
 font)

(defgmethod
 (window+get-theme-default-font-size :class 'window :bind
  "get_theme_default_font_size" :hash 3905245786)
 int)

(defgmethod
 (window+get-window-id :class 'window :bind "get_window_id" :hash 3905245786)
 int)

(defgmethod
 (window+set-accessibility-name :class 'window :bind "set_accessibility_name"
  :hash 83702148)
 :void (name string))

(defgmethod
 (window+get-accessibility-name :class 'window :bind "get_accessibility_name"
  :hash 201670096)
 string)

(defgmethod
 (window+set-accessibility-description :class 'window :bind
  "set_accessibility_description" :hash 83702148)
 :void (description string))

(defgmethod
 (window+get-accessibility-description :class 'window :bind
  "get_accessibility_description" :hash 201670096)
 string)

(defgmethod
 (window+get-focused-window :class 'window :bind "get_focused_window" :hash
  1835468782 :static common-lisp:t)
 window)

(defgmethod
 (window+set-layout-direction :class 'window :bind "set_layout_direction" :hash
  3094704184)
 :void (direction window+layout-direction))

(defgmethod
 (window+get-layout-direction :class 'window :bind "get_layout_direction" :hash
  3909617982)
 window+layout-direction)

(defgmethod
 (window+is-layout-rtl :class 'window :bind "is_layout_rtl" :hash 36873697)
 bool)

(defgmethod
 (window+set-auto-translate :class 'window :bind "set_auto_translate" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (window+is-auto-translating :class 'window :bind "is_auto_translating" :hash
  36873697)
 bool)

(defgmethod
 (window+set-use-font-oversampling :class 'window :bind
  "set_use_font_oversampling" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (window+is-using-font-oversampling :class 'window :bind
  "is_using_font_oversampling" :hash 36873697)
 bool)

(defgmethod (window+popup :class 'window :bind "popup" :hash 1680304321) :void
 (rect rect-2i))

(defgmethod
 (window+popup-on-parent :class 'window :bind "popup_on_parent" :hash
  1763793166)
 :void (parent-rect rect-2i))

(defgmethod
 (window+popup-centered :class 'window :bind "popup_centered" :hash 3447975422)
 :void (minsize vector-2i))

(defgmethod
 (window+popup-centered-ratio :class 'window :bind "popup_centered_ratio" :hash
  1014814997)
 :void (ratio float))

(defgmethod
 (window+popup-centered-clamped :class 'window :bind "popup_centered_clamped"
  :hash 2613752477)
 :void (minsize vector-2i) (fallback-ratio float))

(defgmethod
 (window+popup-exclusive :class 'window :bind "popup_exclusive" :hash
  2134721627)
 :void (from-node node) (rect rect-2i))

(defgmethod
 (window+popup-exclusive-on-parent :class 'window :bind
  "popup_exclusive_on_parent" :hash 2344671043)
 :void (from-node node) (parent-rect rect-2i))

(defgmethod
 (window+popup-exclusive-centered :class 'window :bind
  "popup_exclusive_centered" :hash 3357594017)
 :void (from-node node) (minsize vector-2i))

(defgmethod
 (window+popup-exclusive-centered-ratio :class 'window :bind
  "popup_exclusive_centered_ratio" :hash 2284776287)
 :void (from-node node) (ratio float))

(defgmethod
 (window+popup-exclusive-centered-clamped :class 'window :bind
  "popup_exclusive_centered_clamped" :hash 2612708785)
 :void (from-node node) (minsize vector-2i) (fallback-ratio float))