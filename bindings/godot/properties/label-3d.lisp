(common-lisp:in-package :%godot)


(defgproperty label-3d+pixel-size 'label-3d :get 'label-3d+get-pixel-size :set
 'label-3d+set-pixel-size)

(defgproperty label-3d+offset 'label-3d :get 'label-3d+get-offset :set
 'label-3d+set-offset)

(defgproperty label-3d+billboard 'label-3d :get 'label-3d+get-billboard-mode
 :set 'label-3d+set-billboard-mode)

(defgproperty label-3d+shaded 'label-3d :index 0 :get 'label-3d+get-draw-flag
 :set 'label-3d+set-draw-flag)

(defgproperty label-3d+double-sided 'label-3d :index 1 :get
 'label-3d+get-draw-flag :set 'label-3d+set-draw-flag)

(defgproperty label-3d+no-depth-test 'label-3d :index 2 :get
 'label-3d+get-draw-flag :set 'label-3d+set-draw-flag)

(defgproperty label-3d+fixed-size 'label-3d :index 3 :get
 'label-3d+get-draw-flag :set 'label-3d+set-draw-flag)

(defgproperty label-3d+alpha-cut 'label-3d :get 'label-3d+get-alpha-cut-mode
 :set 'label-3d+set-alpha-cut-mode)

(defgproperty label-3d+alpha-scissor-threshold 'label-3d :get
 'label-3d+get-alpha-scissor-threshold :set
 'label-3d+set-alpha-scissor-threshold)

(defgproperty label-3d+alpha-hash-scale 'label-3d :get
 'label-3d+get-alpha-hash-scale :set 'label-3d+set-alpha-hash-scale)

(defgproperty label-3d+alpha-antialiasing-mode 'label-3d :get
 'label-3d+get-alpha-antialiasing :set 'label-3d+set-alpha-antialiasing)

(defgproperty label-3d+alpha-antialiasing-edge 'label-3d :get
 'label-3d+get-alpha-antialiasing-edge :set
 'label-3d+set-alpha-antialiasing-edge)

(defgproperty label-3d+texture-filter 'label-3d :get
 'label-3d+get-texture-filter :set 'label-3d+set-texture-filter)

(defgproperty label-3d+render-priority 'label-3d :get
 'label-3d+get-render-priority :set 'label-3d+set-render-priority)

(defgproperty label-3d+outline-render-priority 'label-3d :get
 'label-3d+get-outline-render-priority :set
 'label-3d+set-outline-render-priority)

(defgproperty label-3d+modulate 'label-3d :get 'label-3d+get-modulate :set
 'label-3d+set-modulate)

(defgproperty label-3d+outline-modulate 'label-3d :get
 'label-3d+get-outline-modulate :set 'label-3d+set-outline-modulate)

(defgproperty label-3d+text 'label-3d :get 'label-3d+get-text :set
 'label-3d+set-text)

(defgproperty label-3d+font 'label-3d :get 'label-3d+get-font :set
 'label-3d+set-font)

(defgproperty label-3d+font-size 'label-3d :get 'label-3d+get-font-size :set
 'label-3d+set-font-size)

(defgproperty label-3d+outline-size 'label-3d :get 'label-3d+get-outline-size
 :set 'label-3d+set-outline-size)

(defgproperty label-3d+horizontal-alignment 'label-3d :get
 'label-3d+get-horizontal-alignment :set 'label-3d+set-horizontal-alignment)

(defgproperty label-3d+vertical-alignment 'label-3d :get
 'label-3d+get-vertical-alignment :set 'label-3d+set-vertical-alignment)

(defgproperty label-3d+uppercase 'label-3d :get 'label-3d+is-uppercase :set
 'label-3d+set-uppercase)

(defgproperty label-3d+line-spacing 'label-3d :get 'label-3d+get-line-spacing
 :set 'label-3d+set-line-spacing)

(defgproperty label-3d+autowrap-mode 'label-3d :get 'label-3d+get-autowrap-mode
 :set 'label-3d+set-autowrap-mode)

(defgproperty label-3d+autowrap-trim-flags 'label-3d :get
 'label-3d+get-autowrap-trim-flags :set 'label-3d+set-autowrap-trim-flags)

(defgproperty label-3d+justification-flags 'label-3d :get
 'label-3d+get-justification-flags :set 'label-3d+set-justification-flags)

(defgproperty label-3d+width 'label-3d :get 'label-3d+get-width :set
 'label-3d+set-width)

(defgproperty label-3d+text-direction 'label-3d :get
 'label-3d+get-text-direction :set 'label-3d+set-text-direction)

(defgproperty label-3d+language 'label-3d :get 'label-3d+get-language :set
 'label-3d+set-language)

(defgproperty label-3d+structured-text-bidi-override 'label-3d :get
 'label-3d+get-structured-text-bidi-override :set
 'label-3d+set-structured-text-bidi-override)

(defgproperty label-3d+structured-text-bidi-override-options 'label-3d :get
 'label-3d+get-structured-text-bidi-override-options :set
 'label-3d+set-structured-text-bidi-override-options)