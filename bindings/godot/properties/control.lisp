(common-lisp:in-package :%godot)


(defgproperty control+custom-minimum-size 'control :get
 'control+get-custom-minimum-size :set 'control+set-custom-minimum-size)

(defgproperty control+custom-maximum-size 'control :get
 'control+get-custom-maximum-size :set 'control+set-custom-maximum-size)

(defgproperty control+propagate-maximum-size 'control :get
 'control+is-propagating-maximum-size :set 'control+set-propagate-maximum-size)

(defgproperty control+clip-contents 'control :get 'control+is-clipping-contents
 :set 'control+set-clip-contents)

(defgproperty control+layout-mode 'control)

(defgproperty control+anchors-preset 'control)

(defgproperty control+anchor-left 'control :index 0 :get 'control+get-anchor)

(defgproperty control+anchor-top 'control :index 1 :get 'control+get-anchor)

(defgproperty control+anchor-right 'control :index 2 :get 'control+get-anchor)

(defgproperty control+anchor-bottom 'control :index 3 :get 'control+get-anchor)

(defgproperty control+offset-left 'control :index 0 :get 'control+get-offset
 :set 'control+set-offset)

(defgproperty control+offset-top 'control :index 1 :get 'control+get-offset
 :set 'control+set-offset)

(defgproperty control+offset-right 'control :index 2 :get 'control+get-offset
 :set 'control+set-offset)

(defgproperty control+offset-bottom 'control :index 3 :get 'control+get-offset
 :set 'control+set-offset)

(defgproperty control+grow-horizontal 'control :get
 'control+get-h-grow-direction :set 'control+set-h-grow-direction)

(defgproperty control+grow-vertical 'control :get 'control+get-v-grow-direction
 :set 'control+set-v-grow-direction)

(defgproperty control+size 'control :get 'control+get-size)

(defgproperty control+position 'control :get 'control+get-position)

(defgproperty control+global-position 'control :get
 'control+get-global-position)

(defgproperty control+rotation 'control :get 'control+get-rotation :set
 'control+set-rotation)

(defgproperty control+rotation-degrees 'control :get
 'control+get-rotation-degrees :set 'control+set-rotation-degrees)

(defgproperty control+scale 'control :get 'control+get-scale :set
 'control+set-scale)

(defgproperty control+pivot-offset 'control :get 'control+get-pivot-offset :set
 'control+set-pivot-offset)

(defgproperty control+pivot-offset-ratio 'control :get
 'control+get-pivot-offset-ratio :set 'control+set-pivot-offset-ratio)

(defgproperty control+size-flags-horizontal 'control :get
 'control+get-h-size-flags :set 'control+set-h-size-flags)

(defgproperty control+size-flags-vertical 'control :get
 'control+get-v-size-flags :set 'control+set-v-size-flags)

(defgproperty control+size-flags-stretch-ratio 'control :get
 'control+get-stretch-ratio :set 'control+set-stretch-ratio)

(defgproperty control+offset-transform-enabled 'control :get
 'control+is-offset-transform-enabled :set
 'control+set-offset-transform-enabled)

(defgproperty control+offset-transform-position 'control :get
 'control+get-offset-transform-position :set
 'control+set-offset-transform-position)

(defgproperty control+offset-transform-position-ratio 'control :get
 'control+get-offset-transform-position-ratio :set
 'control+set-offset-transform-position-ratio)

(defgproperty control+offset-transform-scale 'control :get
 'control+get-offset-transform-scale :set 'control+set-offset-transform-scale)

(defgproperty control+offset-transform-rotation 'control :get
 'control+get-offset-transform-rotation :set
 'control+set-offset-transform-rotation)

(defgproperty control+offset-transform-pivot 'control :get
 'control+get-offset-transform-pivot :set 'control+set-offset-transform-pivot)

(defgproperty control+offset-transform-pivot-ratio 'control :get
 'control+get-offset-transform-pivot-ratio :set
 'control+set-offset-transform-pivot-ratio)

(defgproperty control+offset-transform-visual-only 'control :get
 'control+is-offset-transform-visual-only :set
 'control+set-offset-transform-visual-only)

(defgproperty control+localize-numeral-system 'control :get
 'control+is-localizing-numeral-system :set
 'control+set-localize-numeral-system)

(defgproperty control+layout-direction 'control :get
 'control+get-layout-direction :set 'control+set-layout-direction)

(defgproperty control+translation-context 'control :get
 'control+get-translation-context :set 'control+set-translation-context)

(defgproperty control+auto-translate 'control :get 'control+is-auto-translating
 :set 'control+set-auto-translate)

(defgproperty control+tooltip-text 'control :get 'control+get-tooltip-text :set
 'control+set-tooltip-text)

(defgproperty control+tooltip-auto-translate-mode 'control :get
 'control+get-tooltip-auto-translate-mode :set
 'control+set-tooltip-auto-translate-mode)

(defgproperty control+focus-neighbor-left 'control :index 0 :get
 'control+get-focus-neighbor :set 'control+set-focus-neighbor)

(defgproperty control+focus-neighbor-top 'control :index 1 :get
 'control+get-focus-neighbor :set 'control+set-focus-neighbor)

(defgproperty control+focus-neighbor-right 'control :index 2 :get
 'control+get-focus-neighbor :set 'control+set-focus-neighbor)

(defgproperty control+focus-neighbor-bottom 'control :index 3 :get
 'control+get-focus-neighbor :set 'control+set-focus-neighbor)

(defgproperty control+focus-next 'control :get 'control+get-focus-next :set
 'control+set-focus-next)

(defgproperty control+focus-previous 'control :get 'control+get-focus-previous
 :set 'control+set-focus-previous)

(defgproperty control+focus-mode 'control :get 'control+get-focus-mode :set
 'control+set-focus-mode)

(defgproperty control+focus-behavior-recursive 'control :get
 'control+get-focus-behavior-recursive :set
 'control+set-focus-behavior-recursive)

(defgproperty control+mouse-filter 'control :get 'control+get-mouse-filter :set
 'control+set-mouse-filter)

(defgproperty control+mouse-behavior-recursive 'control :get
 'control+get-mouse-behavior-recursive :set
 'control+set-mouse-behavior-recursive)

(defgproperty control+mouse-force-pass-scroll-events 'control :get
 'control+is-force-pass-scroll-events :set
 'control+set-force-pass-scroll-events)

(defgproperty control+mouse-default-cursor-shape 'control :get
 'control+get-default-cursor-shape :set 'control+set-default-cursor-shape)

(defgproperty control+shortcut-context 'control :get
 'control+get-shortcut-context :set 'control+set-shortcut-context)

(defgproperty control+accessibility-name 'control :get
 'control+get-accessibility-name :set 'control+set-accessibility-name)

(defgproperty control+accessibility-description 'control :get
 'control+get-accessibility-description :set
 'control+set-accessibility-description)

(defgproperty control+accessibility-live 'control :get
 'control+get-accessibility-live :set 'control+set-accessibility-live)

(defgproperty control+accessibility-controls-nodes 'control :get
 'control+get-accessibility-controls-nodes :set
 'control+set-accessibility-controls-nodes)

(defgproperty control+accessibility-described-by-nodes 'control :get
 'control+get-accessibility-described-by-nodes :set
 'control+set-accessibility-described-by-nodes)

(defgproperty control+accessibility-labeled-by-nodes 'control :get
 'control+get-accessibility-labeled-by-nodes :set
 'control+set-accessibility-labeled-by-nodes)

(defgproperty control+accessibility-flow-to-nodes 'control :get
 'control+get-accessibility-flow-to-nodes :set
 'control+set-accessibility-flow-to-nodes)

(defgproperty control+theme 'control :get 'control+get-theme :set
 'control+set-theme)

(defgproperty control+theme-type-variation 'control :get
 'control+get-theme-type-variation :set 'control+set-theme-type-variation)