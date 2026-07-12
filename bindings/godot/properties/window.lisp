(common-lisp:in-package :%godot)


(defgproperty window+mode 'window :get 'window+get-mode :set 'window+set-mode)

(defgproperty window+title 'window :get 'window+get-title :set
 'window+set-title)

(defgproperty window+initial-position 'window :get 'window+get-initial-position
 :set 'window+set-initial-position)

(defgproperty window+position 'window :get 'window+get-position :set
 'window+set-position)

(defgproperty window+size 'window :get 'window+get-size :set 'window+set-size)

(defgproperty window+current-screen 'window :get 'window+get-current-screen
 :set 'window+set-current-screen)

(defgproperty window+nonclient-area 'window :get 'window+get-nonclient-area
 :set 'window+set-nonclient-area)

(defgproperty window+mouse-passthrough-polygon 'window :get
 'window+get-mouse-passthrough-polygon :set
 'window+set-mouse-passthrough-polygon)

(defgproperty window+visible 'window :get 'window+is-visible :set
 'window+set-visible)

(defgproperty window+wrap-controls 'window :get 'window+is-wrapping-controls
 :set 'window+set-wrap-controls)

(defgproperty window+transient 'window :get 'window+is-transient :set
 'window+set-transient)

(defgproperty window+transient-to-focused 'window :get
 'window+is-transient-to-focused :set 'window+set-transient-to-focused)

(defgproperty window+exclusive 'window :get 'window+is-exclusive :set
 'window+set-exclusive)

(defgproperty window+unresizable 'window :index 0 :get 'window+get-flag :set
 'window+set-flag)

(defgproperty window+borderless 'window :index 1 :get 'window+get-flag :set
 'window+set-flag)

(defgproperty window+always-on-top 'window :index 2 :get 'window+get-flag :set
 'window+set-flag)

(defgproperty window+transparent 'window :index 3 :get 'window+get-flag :set
 'window+set-flag)

(defgproperty window+unfocusable 'window :index 4 :get 'window+get-flag :set
 'window+set-flag)

(defgproperty window+popup-window 'window :index 5 :get 'window+get-flag :set
 'window+set-flag)

(defgproperty window+extend-to-title 'window :index 6 :get 'window+get-flag
 :set 'window+set-flag)

(defgproperty window+mouse-passthrough 'window :index 7 :get 'window+get-flag
 :set 'window+set-flag)

(defgproperty window+sharp-corners 'window :index 8 :get 'window+get-flag :set
 'window+set-flag)

(defgproperty window+exclude-from-capture 'window :index 9 :get
 'window+get-flag :set 'window+set-flag)

(defgproperty window+popup-wm-hint 'window :index 10 :get 'window+get-flag :set
 'window+set-flag)

(defgproperty window+minimize-disabled 'window :index 11 :get 'window+get-flag
 :set 'window+set-flag)

(defgproperty window+maximize-disabled 'window :index 12 :get 'window+get-flag
 :set 'window+set-flag)

(defgproperty window+force-native 'window :get 'window+get-force-native :set
 'window+set-force-native)

(defgproperty window+min-size 'window :get 'window+get-min-size :set
 'window+set-min-size)

(defgproperty window+max-size 'window :get 'window+get-max-size :set
 'window+set-max-size)

(defgproperty window+keep-title-visible 'window :get
 'window+get-keep-title-visible :set 'window+set-keep-title-visible)

(defgproperty window+content-scale-size 'window :get
 'window+get-content-scale-size :set 'window+set-content-scale-size)

(defgproperty window+content-scale-mode 'window :get
 'window+get-content-scale-mode :set 'window+set-content-scale-mode)

(defgproperty window+content-scale-aspect 'window :get
 'window+get-content-scale-aspect :set 'window+set-content-scale-aspect)

(defgproperty window+content-scale-stretch 'window :get
 'window+get-content-scale-stretch :set 'window+set-content-scale-stretch)

(defgproperty window+content-scale-factor 'window :get
 'window+get-content-scale-factor :set 'window+set-content-scale-factor)

(defgproperty window+hdr-output-requested 'window :get
 'window+is-hdr-output-requested :set 'window+set-hdr-output-requested)

(defgproperty window+auto-translate 'window :get 'window+is-auto-translating
 :set 'window+set-auto-translate)

(defgproperty window+accessibility-name 'window :get
 'window+get-accessibility-name :set 'window+set-accessibility-name)

(defgproperty window+accessibility-description 'window :get
 'window+get-accessibility-description :set
 'window+set-accessibility-description)

(defgproperty window+theme 'window :get 'window+get-theme :set
 'window+set-theme)

(defgproperty window+theme-type-variation 'window :get
 'window+get-theme-type-variation :set 'window+set-theme-type-variation)