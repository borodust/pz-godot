(common-lisp:in-package :%godot)


(defgmethod
 (gradient+add-point :class 'gradient :bind "add_point" :hash 3629403827) :void
 (offset float) (color color))

(defgmethod
 (gradient+remove-point :class 'gradient :bind "remove_point" :hash 1286410249)
 :void (point int))

(defgmethod
 (gradient+set-offset :class 'gradient :bind "set_offset" :hash 1602489585)
 :void (point int) (offset float))

(defgmethod
 (gradient+get-offset :class 'gradient :bind "get_offset" :hash 4025615559)
 float (point int))

(defgmethod
 (gradient+reverse :class 'gradient :bind "reverse" :hash 3218959716) :void)

(defgmethod
 (gradient+set-color :class 'gradient :bind "set_color" :hash 2878471219) :void
 (point int) (color color))

(defgmethod
 (gradient+get-color :class 'gradient :bind "get_color" :hash 2624840992) color
 (point int))

(defgmethod (gradient+sample :class 'gradient :bind "sample" :hash 1250405064)
 color (offset float))

(defgmethod
 (gradient+get-point-count :class 'gradient :bind "get_point_count" :hash
  3905245786)
 int)

(defgmethod
 (gradient+set-offsets :class 'gradient :bind "set_offsets" :hash 2899603908)
 :void (offsets packed-float-32array))

(defgmethod
 (gradient+get-offsets :class 'gradient :bind "get_offsets" :hash 675695659)
 packed-float-32array)

(defgmethod
 (gradient+set-colors :class 'gradient :bind "set_colors" :hash 3546319833)
 :void (colors packed-color-array))

(defgmethod
 (gradient+get-colors :class 'gradient :bind "get_colors" :hash 1392750486)
 packed-color-array)

(defgmethod
 (gradient+set-interpolation-mode :class 'gradient :bind
  "set_interpolation_mode" :hash 1971444490)
 :void (interpolation-mode gradient+interpolation-mode))

(defgmethod
 (gradient+get-interpolation-mode :class 'gradient :bind
  "get_interpolation_mode" :hash 3674172981)
 gradient+interpolation-mode)

(defgmethod
 (gradient+set-interpolation-color-space :class 'gradient :bind
  "set_interpolation_color_space" :hash 3685995981)
 :void (interpolation-color-space gradient+color-space))

(defgmethod
 (gradient+get-interpolation-color-space :class 'gradient :bind
  "get_interpolation_color_space" :hash 1538296000)
 gradient+color-space)