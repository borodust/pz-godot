(common-lisp:in-package :%godot)


(defgmethod
 (spin-box+set-horizontal-alignment :class 'spin-box :bind
  "set_horizontal_alignment" :hash 2312603777)
 :void (alignment horizontal-alignment))

(defgmethod
 (spin-box+get-horizontal-alignment :class 'spin-box :bind
  "get_horizontal_alignment" :hash 341400642)
 horizontal-alignment)

(defgmethod
 (spin-box+set-suffix :class 'spin-box :bind "set_suffix" :hash 83702148) :void
 (suffix string))

(defgmethod
 (spin-box+get-suffix :class 'spin-box :bind "get_suffix" :hash 201670096)
 string)

(defgmethod
 (spin-box+set-prefix :class 'spin-box :bind "set_prefix" :hash 83702148) :void
 (prefix string))

(defgmethod
 (spin-box+get-prefix :class 'spin-box :bind "get_prefix" :hash 201670096)
 string)

(defgmethod
 (spin-box+set-editable :class 'spin-box :bind "set_editable" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (spin-box+set-custom-arrow-step :class 'spin-box :bind "set_custom_arrow_step"
  :hash 373806689)
 :void (arrow-step float))

(defgmethod
 (spin-box+get-custom-arrow-step :class 'spin-box :bind "get_custom_arrow_step"
  :hash 1740695150)
 float)

(defgmethod
 (spin-box+set-custom-arrow-round :class 'spin-box :bind
  "set_custom_arrow_round" :hash 2586408642)
 :void (round bool))

(defgmethod
 (spin-box+is-custom-arrow-rounding :class 'spin-box :bind
  "is_custom_arrow_rounding" :hash 36873697)
 bool)

(defgmethod
 (spin-box+is-editable :class 'spin-box :bind "is_editable" :hash 36873697)
 bool)

(defgmethod
 (spin-box+set-update-on-text-changed :class 'spin-box :bind
  "set_update_on_text_changed" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (spin-box+get-update-on-text-changed :class 'spin-box :bind
  "get_update_on_text_changed" :hash 36873697)
 bool)

(defgmethod
 (spin-box+set-select-all-on-focus :class 'spin-box :bind
  "set_select_all_on_focus" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (spin-box+is-select-all-on-focus :class 'spin-box :bind
  "is_select_all_on_focus" :hash 36873697)
 bool)

(defgmethod (spin-box+apply :class 'spin-box :bind "apply" :hash 3218959716)
 :void)

(defgmethod
 (spin-box+get-line-edit :class 'spin-box :bind "get_line_edit" :hash
  4071694264)
 line-edit)