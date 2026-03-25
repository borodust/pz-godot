(common-lisp:in-package :%godot)


(defgproperty system-font+font-names 'system-font :get
 'system-font+get-font-names :set 'system-font+set-font-names)

(defgproperty system-font+font-italic 'system-font :get
 'system-font+get-font-italic :set 'system-font+set-font-italic)

(defgproperty system-font+font-weight 'system-font :set
 'system-font+set-font-weight)

(defgproperty system-font+font-stretch 'system-font :set
 'system-font+set-font-stretch)

(defgproperty system-font+antialiasing 'system-font :get
 'system-font+get-antialiasing :set 'system-font+set-antialiasing)

(defgproperty system-font+generate-mipmaps 'system-font :get
 'system-font+get-generate-mipmaps :set 'system-font+set-generate-mipmaps)

(defgproperty system-font+disable-embedded-bitmaps 'system-font :get
 'system-font+get-disable-embedded-bitmaps :set
 'system-font+set-disable-embedded-bitmaps)

(defgproperty system-font+allow-system-fallback 'system-font :get
 'system-font+is-allow-system-fallback :set
 'system-font+set-allow-system-fallback)

(defgproperty system-font+force-autohinter 'system-font :get
 'system-font+is-force-autohinter :set 'system-font+set-force-autohinter)

(defgproperty system-font+modulate-color-glyphs 'system-font :get
 'system-font+is-modulate-color-glyphs :set
 'system-font+set-modulate-color-glyphs)

(defgproperty system-font+hinting 'system-font :get 'system-font+get-hinting
 :set 'system-font+set-hinting)

(defgproperty system-font+subpixel-positioning 'system-font :get
 'system-font+get-subpixel-positioning :set
 'system-font+set-subpixel-positioning)

(defgproperty system-font+keep-rounding-remainders 'system-font :get
 'system-font+get-keep-rounding-remainders :set
 'system-font+set-keep-rounding-remainders)

(defgproperty system-font+multichannel-signed-distance-field 'system-font :get
 'system-font+is-multichannel-signed-distance-field :set
 'system-font+set-multichannel-signed-distance-field)

(defgproperty system-font+msdf-pixel-range 'system-font :get
 'system-font+get-msdf-pixel-range :set 'system-font+set-msdf-pixel-range)

(defgproperty system-font+msdf-size 'system-font :get
 'system-font+get-msdf-size :set 'system-font+set-msdf-size)

(defgproperty system-font+oversampling 'system-font :get
 'system-font+get-oversampling :set 'system-font+set-oversampling)