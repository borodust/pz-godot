(common-lisp:in-package :%godot)


(defgmethod
 (font-variation+set-base-font :class 'font-variation :bind "set_base_font"
  :hash 1262170328)
 :void (font font))

(defgmethod
 (font-variation+get-base-font :class 'font-variation :bind "get_base_font"
  :hash 3229501585)
 font)

(defgmethod
 (font-variation+set-variation-opentype :class 'font-variation :bind
  "set_variation_opentype" :hash 4155329257)
 :void (coords dictionary))

(defgmethod
 (font-variation+get-variation-opentype :class 'font-variation :bind
  "get_variation_opentype" :hash 3102165223)
 dictionary)

(defgmethod
 (font-variation+set-variation-embolden :class 'font-variation :bind
  "set_variation_embolden" :hash 373806689)
 :void (strength float))

(defgmethod
 (font-variation+get-variation-embolden :class 'font-variation :bind
  "get_variation_embolden" :hash 1740695150)
 float)

(defgmethod
 (font-variation+set-variation-face-index :class 'font-variation :bind
  "set_variation_face_index" :hash 1286410249)
 :void (face-index int))

(defgmethod
 (font-variation+get-variation-face-index :class 'font-variation :bind
  "get_variation_face_index" :hash 3905245786)
 int)

(defgmethod
 (font-variation+set-variation-transform :class 'font-variation :bind
  "set_variation_transform" :hash 2761652528)
 :void (transform transform-2d))

(defgmethod
 (font-variation+get-variation-transform :class 'font-variation :bind
  "get_variation_transform" :hash 3814499831)
 transform-2d)

(defgmethod
 (font-variation+set-opentype-features :class 'font-variation :bind
  "set_opentype_features" :hash 4155329257)
 :void (features dictionary))

(defgmethod
 (font-variation+set-spacing :class 'font-variation :bind "set_spacing" :hash
  3122339690)
 :void (spacing text-server+spacing-type) (value int))

(defgmethod
 (font-variation+set-baseline-offset :class 'font-variation :bind
  "set_baseline_offset" :hash 373806689)
 :void (baseline-offset float))

(defgmethod
 (font-variation+get-baseline-offset :class 'font-variation :bind
  "get_baseline_offset" :hash 1740695150)
 float)