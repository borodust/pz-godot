(common-lisp:in-package :%godot)


(defgmethod
 (label+set-horizontal-alignment :class 'label :bind "set_horizontal_alignment"
  :hash 2312603777)
 :void (alignment horizontal-alignment))

(defgmethod
 (label+get-horizontal-alignment :class 'label :bind "get_horizontal_alignment"
  :hash 341400642)
 horizontal-alignment)

(defgmethod
 (label+set-vertical-alignment :class 'label :bind "set_vertical_alignment"
  :hash 1796458609)
 :void (alignment vertical-alignment))

(defgmethod
 (label+get-vertical-alignment :class 'label :bind "get_vertical_alignment"
  :hash 3274884059)
 vertical-alignment)

(defgmethod (label+set-text :class 'label :bind "set_text" :hash 83702148)
 :void (text string))

(defgmethod (label+get-text :class 'label :bind "get_text" :hash 201670096)
 string)

(defgmethod
 (label+set-label-settings :class 'label :bind "set_label_settings" :hash
  1030653839)
 :void (settings label-settings))

(defgmethod
 (label+get-label-settings :class 'label :bind "get_label_settings" :hash
  826676056)
 label-settings)

(defgmethod
 (label+set-text-direction :class 'label :bind "set_text_direction" :hash
  119160795)
 :void (direction control+text-direction))

(defgmethod
 (label+get-text-direction :class 'label :bind "get_text_direction" :hash
  797257663)
 control+text-direction)

(defgmethod
 (label+set-language :class 'label :bind "set_language" :hash 83702148) :void
 (language string))

(defgmethod
 (label+get-language :class 'label :bind "get_language" :hash 201670096) string)

(defgmethod
 (label+set-paragraph-separator :class 'label :bind "set_paragraph_separator"
  :hash 83702148)
 :void (paragraph-separator string))

(defgmethod
 (label+get-paragraph-separator :class 'label :bind "get_paragraph_separator"
  :hash 201670096)
 string)

(defgmethod
 (label+set-autowrap-mode :class 'label :bind "set_autowrap_mode" :hash
  3289138044)
 :void (autowrap-mode text-server+autowrap-mode))

(defgmethod
 (label+get-autowrap-mode :class 'label :bind "get_autowrap_mode" :hash
  1549071663)
 text-server+autowrap-mode)

(defgmethod
 (label+set-autowrap-trim-flags :class 'label :bind "set_autowrap_trim_flags"
  :hash 2809697122)
 :void (autowrap-trim-flags text-server+line-break-flag))

(defgmethod
 (label+get-autowrap-trim-flags :class 'label :bind "get_autowrap_trim_flags"
  :hash 2340632602)
 text-server+line-break-flag)

(defgmethod
 (label+set-justification-flags :class 'label :bind "set_justification_flags"
  :hash 2877345813)
 :void (justification-flags text-server+justification-flag))

(defgmethod
 (label+get-justification-flags :class 'label :bind "get_justification_flags"
  :hash 1583363614)
 text-server+justification-flag)

(defgmethod
 (label+set-clip-text :class 'label :bind "set_clip_text" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (label+is-clipping-text :class 'label :bind "is_clipping_text" :hash 36873697)
 bool)

(defgmethod
 (label+set-tab-stops :class 'label :bind "set_tab_stops" :hash 2899603908)
 :void (tab-stops packed-float-32array))

(defgmethod
 (label+get-tab-stops :class 'label :bind "get_tab_stops" :hash 675695659)
 packed-float-32array)

(defgmethod
 (label+set-text-overrun-behavior :class 'label :bind
  "set_text_overrun_behavior" :hash 1008890932)
 :void (overrun-behavior text-server+overrun-behavior))

(defgmethod
 (label+get-text-overrun-behavior :class 'label :bind
  "get_text_overrun_behavior" :hash 3779142101)
 text-server+overrun-behavior)

(defgmethod
 (label+set-ellipsis-char :class 'label :bind "set_ellipsis_char" :hash
  83702148)
 :void (char string))

(defgmethod
 (label+get-ellipsis-char :class 'label :bind "get_ellipsis_char" :hash
  201670096)
 string)

(defgmethod
 (label+set-uppercase :class 'label :bind "set_uppercase" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (label+is-uppercase :class 'label :bind "is_uppercase" :hash 36873697) bool)

(defgmethod
 (label+get-line-height :class 'label :bind "get_line_height" :hash 181039630)
 int (line int))

(defgmethod
 (label+get-line-count :class 'label :bind "get_line_count" :hash 3905245786)
 int)

(defgmethod
 (label+get-visible-line-count :class 'label :bind "get_visible_line_count"
  :hash 3905245786)
 int)

(defgmethod
 (label+get-total-character-count :class 'label :bind
  "get_total_character_count" :hash 3905245786)
 int)

(defgmethod
 (label+set-visible-characters :class 'label :bind "set_visible_characters"
  :hash 1286410249)
 :void (amount int))

(defgmethod
 (label+get-visible-characters :class 'label :bind "get_visible_characters"
  :hash 3905245786)
 int)

(defgmethod
 (label+get-visible-characters-behavior :class 'label :bind
  "get_visible_characters_behavior" :hash 258789322)
 text-server+visible-characters-behavior)

(defgmethod
 (label+set-visible-characters-behavior :class 'label :bind
  "set_visible_characters_behavior" :hash 3383839701)
 :void (behavior text-server+visible-characters-behavior))

(defgmethod
 (label+set-visible-ratio :class 'label :bind "set_visible_ratio" :hash
  373806689)
 :void (ratio float))

(defgmethod
 (label+get-visible-ratio :class 'label :bind "get_visible_ratio" :hash
  1740695150)
 float)

(defgmethod
 (label+set-lines-skipped :class 'label :bind "set_lines_skipped" :hash
  1286410249)
 :void (lines-skipped int))

(defgmethod
 (label+get-lines-skipped :class 'label :bind "get_lines_skipped" :hash
  3905245786)
 int)

(defgmethod
 (label+set-max-lines-visible :class 'label :bind "set_max_lines_visible" :hash
  1286410249)
 :void (lines-visible int))

(defgmethod
 (label+get-max-lines-visible :class 'label :bind "get_max_lines_visible" :hash
  3905245786)
 int)

(defgmethod
 (label+set-structured-text-bidi-override :class 'label :bind
  "set_structured_text_bidi_override" :hash 55961453)
 :void (parser text-server+structured-text-parser))

(defgmethod
 (label+get-structured-text-bidi-override :class 'label :bind
  "get_structured_text_bidi_override" :hash 3385126229)
 text-server+structured-text-parser)

(defgmethod
 (label+set-structured-text-bidi-override-options :class 'label :bind
  "set_structured_text_bidi_override_options" :hash 381264803)
 :void (args array))

(defgmethod
 (label+get-structured-text-bidi-override-options :class 'label :bind
  "get_structured_text_bidi_override_options" :hash 3995934104)
 array)

(defgmethod
 (label+get-character-bounds :class 'label :bind "get_character_bounds" :hash
  3327874267)
 rect-2 (pos int))