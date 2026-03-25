(common-lisp:in-package :%godot)


(defgmethod (color+to-argb32 :class 'color :bind "to_argb32" :hash 3173160232)
 int)

(defgmethod (color+to-abgr32 :class 'color :bind "to_abgr32" :hash 3173160232)
 int)

(defgmethod (color+to-rgba32 :class 'color :bind "to_rgba32" :hash 3173160232)
 int)

(defgmethod (color+to-argb64 :class 'color :bind "to_argb64" :hash 3173160232)
 int)

(defgmethod (color+to-abgr64 :class 'color :bind "to_abgr64" :hash 3173160232)
 int)

(defgmethod (color+to-rgba64 :class 'color :bind "to_rgba64" :hash 3173160232)
 int)

(defgmethod (color+to-html :class 'color :bind "to_html" :hash 3429816538)
 string (with-alpha bool))

(defgmethod (color+clamp :class 'color :bind "clamp" :hash 105651410) color
 (min color) (max color))

(defgmethod (color+inverted :class 'color :bind "inverted" :hash 3334027602)
 color)

(defgmethod (color+lerp :class 'color :bind "lerp" :hash 402949615) color
 (to color) (weight float))

(defgmethod (color+lightened :class 'color :bind "lightened" :hash 1466039168)
 color (amount float))

(defgmethod (color+darkened :class 'color :bind "darkened" :hash 1466039168)
 color (amount float))

(defgmethod (color+blend :class 'color :bind "blend" :hash 3803690977) color
 (over color))

(defgmethod
 (color+get-luminance :class 'color :bind "get_luminance" :hash 466405837)
 float)

(defgmethod
 (color+srgb-to-linear :class 'color :bind "srgb_to_linear" :hash 3334027602)
 color)

(defgmethod
 (color+linear-to-srgb :class 'color :bind "linear_to_srgb" :hash 3334027602)
 color)

(defgmethod
 (color+is-equal-approx :class 'color :bind "is_equal_approx" :hash 3167426256)
 bool (to color))

(defgmethod
 (color+hex :class 'color :bind "hex" :hash 351421375 :static common-lisp:t)
 color (hex int))

(defgmethod
 (color+hex64 :class 'color :bind "hex64" :hash 351421375 :static
  common-lisp:t)
 color (hex int))

(defgmethod
 (color+html :class 'color :bind "html" :hash 2500054655 :static common-lisp:t)
 color (rgba string))

(defgmethod
 (color+html-is-valid :class 'color :bind "html_is_valid" :hash 2942997125
  :static common-lisp:t)
 bool (color string))

(defgmethod
 (color+from-string :class 'color :bind "from_string" :hash 3755044230 :static
  common-lisp:t)
 color (str string) (default color))

(defgmethod
 (color+from-hsv :class 'color :bind "from_hsv" :hash 1573799446 :static
  common-lisp:t)
 color (h float) (s float) (v float) (alpha float))

(defgmethod
 (color+from-ok-hsl :class 'color :bind "from_ok_hsl" :hash 1573799446 :static
  common-lisp:t)
 color (h float) (s float) (l float) (alpha float))

(defgmethod
 (color+from-rgbe9995 :class 'color :bind "from_rgbe9995" :hash 351421375
  :static common-lisp:t)
 color (rgbe int))

(defgmethod
 (color+from-rgba8 :class 'color :bind "from_rgba8" :hash 3072934735 :static
  common-lisp:t)
 color (r8 int) (g8 int) (b8 int) (a8 int))