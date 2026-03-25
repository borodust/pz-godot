(common-lisp:in-package :%godot)


(defgmethod (button+set-text :class 'button :bind "set_text" :hash 83702148)
 :void (text string))

(defgmethod (button+get-text :class 'button :bind "get_text" :hash 201670096)
 string)

(defgmethod
 (button+set-text-overrun-behavior :class 'button :bind
  "set_text_overrun_behavior" :hash 1008890932)
 :void (overrun-behavior text-server+overrun-behavior))

(defgmethod
 (button+get-text-overrun-behavior :class 'button :bind
  "get_text_overrun_behavior" :hash 3779142101)
 text-server+overrun-behavior)

(defgmethod
 (button+set-autowrap-mode :class 'button :bind "set_autowrap_mode" :hash
  3289138044)
 :void (autowrap-mode text-server+autowrap-mode))

(defgmethod
 (button+get-autowrap-mode :class 'button :bind "get_autowrap_mode" :hash
  1549071663)
 text-server+autowrap-mode)

(defgmethod
 (button+set-autowrap-trim-flags :class 'button :bind "set_autowrap_trim_flags"
  :hash 2809697122)
 :void (autowrap-trim-flags text-server+line-break-flag))

(defgmethod
 (button+get-autowrap-trim-flags :class 'button :bind "get_autowrap_trim_flags"
  :hash 2340632602)
 text-server+line-break-flag)

(defgmethod
 (button+set-text-direction :class 'button :bind "set_text_direction" :hash
  119160795)
 :void (direction control+text-direction))

(defgmethod
 (button+get-text-direction :class 'button :bind "get_text_direction" :hash
  797257663)
 control+text-direction)

(defgmethod
 (button+set-language :class 'button :bind "set_language" :hash 83702148) :void
 (language string))

(defgmethod
 (button+get-language :class 'button :bind "get_language" :hash 201670096)
 string)

(defgmethod
 (button+set-button-icon :class 'button :bind "set_button_icon" :hash
  4051416890)
 :void (texture texture-2d))

(defgmethod
 (button+get-button-icon :class 'button :bind "get_button_icon" :hash
  3635182373)
 texture-2d)

(defgmethod (button+set-flat :class 'button :bind "set_flat" :hash 2586408642)
 :void (enabled bool))

(defgmethod (button+is-flat :class 'button :bind "is_flat" :hash 36873697) bool)

(defgmethod
 (button+set-clip-text :class 'button :bind "set_clip_text" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (button+get-clip-text :class 'button :bind "get_clip_text" :hash 36873697)
 bool)

(defgmethod
 (button+set-text-alignment :class 'button :bind "set_text_alignment" :hash
  2312603777)
 :void (alignment horizontal-alignment))

(defgmethod
 (button+get-text-alignment :class 'button :bind "get_text_alignment" :hash
  341400642)
 horizontal-alignment)

(defgmethod
 (button+set-icon-alignment :class 'button :bind "set_icon_alignment" :hash
  2312603777)
 :void (icon-alignment horizontal-alignment))

(defgmethod
 (button+get-icon-alignment :class 'button :bind "get_icon_alignment" :hash
  341400642)
 horizontal-alignment)

(defgmethod
 (button+set-vertical-icon-alignment :class 'button :bind
  "set_vertical_icon_alignment" :hash 1796458609)
 :void (vertical-icon-alignment vertical-alignment))

(defgmethod
 (button+get-vertical-icon-alignment :class 'button :bind
  "get_vertical_icon_alignment" :hash 3274884059)
 vertical-alignment)

(defgmethod
 (button+set-expand-icon :class 'button :bind "set_expand_icon" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (button+is-expand-icon :class 'button :bind "is_expand_icon" :hash 36873697)
 bool)