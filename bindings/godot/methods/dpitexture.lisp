(common-lisp:in-package :%godot)


(defgmethod
 (dpitexture+create-from-string :class 'dpitexture :bind "create_from_string"
  :hash 755140520 :static common-lisp:t)
 dpitexture (source string) (scale float) (saturation float)
 (color-map dictionary))

(defgmethod
 (dpitexture+set-source :class 'dpitexture :bind "set_source" :hash 83702148)
 :void (source string))

(defgmethod
 (dpitexture+get-source :class 'dpitexture :bind "get_source" :hash 201670096)
 string)

(defgmethod
 (dpitexture+set-base-scale :class 'dpitexture :bind "set_base_scale" :hash
  373806689)
 :void (base-scale float))

(defgmethod
 (dpitexture+get-base-scale :class 'dpitexture :bind "get_base_scale" :hash
  1740695150)
 float)

(defgmethod
 (dpitexture+set-saturation :class 'dpitexture :bind "set_saturation" :hash
  373806689)
 :void (saturation float))

(defgmethod
 (dpitexture+get-saturation :class 'dpitexture :bind "get_saturation" :hash
  1740695150)
 float)

(defgmethod
 (dpitexture+set-color-map :class 'dpitexture :bind "set_color_map" :hash
  4155329257)
 :void (color-map dictionary))

(defgmethod
 (dpitexture+get-color-map :class 'dpitexture :bind "get_color_map" :hash
  3102165223)
 dictionary)

(defgmethod
 (dpitexture+set-size-override :class 'dpitexture :bind "set_size_override"
  :hash 1130785943)
 :void (size vector-2i))

(defgmethod
 (dpitexture+get-scaled-rid :class 'dpitexture :bind "get_scaled_rid" :hash
  2944877500)
 rid)