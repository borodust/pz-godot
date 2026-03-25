(common-lisp:in-package :%godot)


(defgproperty font-file+data 'font-file :get 'font-file+get-data :set
 'font-file+set-data)

(defgproperty font-file+generate-mipmaps 'font-file :get
 'font-file+get-generate-mipmaps :set 'font-file+set-generate-mipmaps)

(defgproperty font-file+disable-embedded-bitmaps 'font-file :get
 'font-file+get-disable-embedded-bitmaps :set
 'font-file+set-disable-embedded-bitmaps)

(defgproperty font-file+antialiasing 'font-file :get
 'font-file+get-antialiasing :set 'font-file+set-antialiasing)

(defgproperty font-file+font-name 'font-file :set 'font-file+set-font-name)

(defgproperty font-file+style-name 'font-file :set
 'font-file+set-font-style-name)

(defgproperty font-file+font-style 'font-file :set 'font-file+set-font-style)

(defgproperty font-file+font-weight 'font-file :set 'font-file+set-font-weight)

(defgproperty font-file+font-stretch 'font-file :set
 'font-file+set-font-stretch)

(defgproperty font-file+subpixel-positioning 'font-file :get
 'font-file+get-subpixel-positioning :set 'font-file+set-subpixel-positioning)

(defgproperty font-file+keep-rounding-remainders 'font-file :get
 'font-file+get-keep-rounding-remainders :set
 'font-file+set-keep-rounding-remainders)

(defgproperty font-file+multichannel-signed-distance-field 'font-file :get
 'font-file+is-multichannel-signed-distance-field :set
 'font-file+set-multichannel-signed-distance-field)

(defgproperty font-file+msdf-pixel-range 'font-file :get
 'font-file+get-msdf-pixel-range :set 'font-file+set-msdf-pixel-range)

(defgproperty font-file+msdf-size 'font-file :get 'font-file+get-msdf-size :set
 'font-file+set-msdf-size)

(defgproperty font-file+allow-system-fallback 'font-file :get
 'font-file+is-allow-system-fallback :set 'font-file+set-allow-system-fallback)

(defgproperty font-file+force-autohinter 'font-file :get
 'font-file+is-force-autohinter :set 'font-file+set-force-autohinter)

(defgproperty font-file+modulate-color-glyphs 'font-file :get
 'font-file+is-modulate-color-glyphs :set 'font-file+set-modulate-color-glyphs)

(defgproperty font-file+hinting 'font-file :get 'font-file+get-hinting :set
 'font-file+set-hinting)

(defgproperty font-file+fixed-size 'font-file :get 'font-file+get-fixed-size
 :set 'font-file+set-fixed-size)

(defgproperty font-file+fixed-size-scale-mode 'font-file :get
 'font-file+get-fixed-size-scale-mode :set 'font-file+set-fixed-size-scale-mode)

(defgproperty font-file+opentype-feature-overrides 'font-file :get
 'font-file+get-opentype-feature-overrides :set
 'font-file+set-opentype-feature-overrides)

(defgproperty font-file+oversampling 'font-file :get
 'font-file+get-oversampling :set 'font-file+set-oversampling)