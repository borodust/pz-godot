(common-lisp:in-package :%godot)


(defgmethod
 (label-settings+set-line-spacing :class 'label-settings :bind
  "set_line_spacing" :hash 373806689)
 :void (spacing float))

(defgmethod
 (label-settings+get-line-spacing :class 'label-settings :bind
  "get_line_spacing" :hash 1740695150)
 float)

(defgmethod
 (label-settings+set-paragraph-spacing :class 'label-settings :bind
  "set_paragraph_spacing" :hash 373806689)
 :void (spacing float))

(defgmethod
 (label-settings+get-paragraph-spacing :class 'label-settings :bind
  "get_paragraph_spacing" :hash 1740695150)
 float)

(defgmethod
 (label-settings+set-font :class 'label-settings :bind "set_font" :hash
  1262170328)
 :void (font font))

(defgmethod
 (label-settings+get-font :class 'label-settings :bind "get_font" :hash
  3229501585)
 font)

(defgmethod
 (label-settings+set-font-size :class 'label-settings :bind "set_font_size"
  :hash 1286410249)
 :void (size int))

(defgmethod
 (label-settings+get-font-size :class 'label-settings :bind "get_font_size"
  :hash 3905245786)
 int)

(defgmethod
 (label-settings+set-font-color :class 'label-settings :bind "set_font_color"
  :hash 2920490490)
 :void (color color))

(defgmethod
 (label-settings+get-font-color :class 'label-settings :bind "get_font_color"
  :hash 3444240500)
 color)

(defgmethod
 (label-settings+set-outline-size :class 'label-settings :bind
  "set_outline_size" :hash 1286410249)
 :void (size int))

(defgmethod
 (label-settings+get-outline-size :class 'label-settings :bind
  "get_outline_size" :hash 3905245786)
 int)

(defgmethod
 (label-settings+set-outline-color :class 'label-settings :bind
  "set_outline_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (label-settings+get-outline-color :class 'label-settings :bind
  "get_outline_color" :hash 3444240500)
 color)

(defgmethod
 (label-settings+set-shadow-size :class 'label-settings :bind "set_shadow_size"
  :hash 1286410249)
 :void (size int))

(defgmethod
 (label-settings+get-shadow-size :class 'label-settings :bind "get_shadow_size"
  :hash 3905245786)
 int)

(defgmethod
 (label-settings+set-shadow-color :class 'label-settings :bind
  "set_shadow_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (label-settings+get-shadow-color :class 'label-settings :bind
  "get_shadow_color" :hash 3444240500)
 color)

(defgmethod
 (label-settings+set-shadow-offset :class 'label-settings :bind
  "set_shadow_offset" :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (label-settings+get-shadow-offset :class 'label-settings :bind
  "get_shadow_offset" :hash 3341600327)
 vector-2)

(defgmethod
 (label-settings+get-stacked-outline-count :class 'label-settings :bind
  "get_stacked_outline_count" :hash 3905245786)
 int)

(defgmethod
 (label-settings+set-stacked-outline-count :class 'label-settings :bind
  "set_stacked_outline_count" :hash 1286410249)
 :void (count int))

(defgmethod
 (label-settings+add-stacked-outline :class 'label-settings :bind
  "add_stacked_outline" :hash 1025054187)
 :void (index int))

(defgmethod
 (label-settings+move-stacked-outline :class 'label-settings :bind
  "move_stacked_outline" :hash 3937882851)
 :void (from-index int) (to-position int))

(defgmethod
 (label-settings+remove-stacked-outline :class 'label-settings :bind
  "remove_stacked_outline" :hash 1286410249)
 :void (index int))

(defgmethod
 (label-settings+set-stacked-outline-size :class 'label-settings :bind
  "set_stacked_outline_size" :hash 3937882851)
 :void (index int) (size int))

(defgmethod
 (label-settings+get-stacked-outline-size :class 'label-settings :bind
  "get_stacked_outline_size" :hash 923996154)
 int (index int))

(defgmethod
 (label-settings+set-stacked-outline-color :class 'label-settings :bind
  "set_stacked_outline_color" :hash 2878471219)
 :void (index int) (color color))

(defgmethod
 (label-settings+get-stacked-outline-color :class 'label-settings :bind
  "get_stacked_outline_color" :hash 3457211756)
 color (index int))

(defgmethod
 (label-settings+get-stacked-shadow-count :class 'label-settings :bind
  "get_stacked_shadow_count" :hash 3905245786)
 int)

(defgmethod
 (label-settings+set-stacked-shadow-count :class 'label-settings :bind
  "set_stacked_shadow_count" :hash 1286410249)
 :void (count int))

(defgmethod
 (label-settings+add-stacked-shadow :class 'label-settings :bind
  "add_stacked_shadow" :hash 1025054187)
 :void (index int))

(defgmethod
 (label-settings+move-stacked-shadow :class 'label-settings :bind
  "move_stacked_shadow" :hash 3937882851)
 :void (from-index int) (to-position int))

(defgmethod
 (label-settings+remove-stacked-shadow :class 'label-settings :bind
  "remove_stacked_shadow" :hash 1286410249)
 :void (index int))

(defgmethod
 (label-settings+set-stacked-shadow-offset :class 'label-settings :bind
  "set_stacked_shadow_offset" :hash 163021252)
 :void (index int) (offset vector-2))

(defgmethod
 (label-settings+get-stacked-shadow-offset :class 'label-settings :bind
  "get_stacked_shadow_offset" :hash 2299179447)
 vector-2 (index int))

(defgmethod
 (label-settings+set-stacked-shadow-color :class 'label-settings :bind
  "set_stacked_shadow_color" :hash 2878471219)
 :void (index int) (color color))

(defgmethod
 (label-settings+get-stacked-shadow-color :class 'label-settings :bind
  "get_stacked_shadow_color" :hash 3457211756)
 color (index int))

(defgmethod
 (label-settings+set-stacked-shadow-outline-size :class 'label-settings :bind
  "set_stacked_shadow_outline_size" :hash 3937882851)
 :void (index int) (size int))

(defgmethod
 (label-settings+get-stacked-shadow-outline-size :class 'label-settings :bind
  "get_stacked_shadow_outline_size" :hash 923996154)
 int (index int))