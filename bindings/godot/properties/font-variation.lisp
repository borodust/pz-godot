(common-lisp:in-package :%godot)


(defgproperty font-variation+base-font 'font-variation :get
 'font-variation+get-base-font :set 'font-variation+set-base-font)

(defgproperty font-variation+variation-opentype 'font-variation :get
 'font-variation+get-variation-opentype :set
 'font-variation+set-variation-opentype)

(defgproperty font-variation+variation-face-index 'font-variation :get
 'font-variation+get-variation-face-index :set
 'font-variation+set-variation-face-index)

(defgproperty font-variation+variation-embolden 'font-variation :get
 'font-variation+get-variation-embolden :set
 'font-variation+set-variation-embolden)

(defgproperty font-variation+variation-transform 'font-variation :get
 'font-variation+get-variation-transform :set
 'font-variation+set-variation-transform)

(defgproperty font-variation+opentype-features 'font-variation :set
 'font-variation+set-opentype-features)

(defgproperty font-variation+spacing-glyph 'font-variation :index 0 :set
 'font-variation+set-spacing)

(defgproperty font-variation+spacing-space 'font-variation :index 1 :set
 'font-variation+set-spacing)

(defgproperty font-variation+spacing-top 'font-variation :index 2 :set
 'font-variation+set-spacing)

(defgproperty font-variation+spacing-bottom 'font-variation :index 3 :set
 'font-variation+set-spacing)

(defgproperty font-variation+baseline-offset 'font-variation :get
 'font-variation+get-baseline-offset :set 'font-variation+set-baseline-offset)