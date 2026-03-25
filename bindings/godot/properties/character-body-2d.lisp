(common-lisp:in-package :%godot)


(defgproperty character-body-2d+motion-mode 'character-body-2d :get
 'character-body-2d+get-motion-mode :set 'character-body-2d+set-motion-mode)

(defgproperty character-body-2d+up-direction 'character-body-2d :get
 'character-body-2d+get-up-direction :set 'character-body-2d+set-up-direction)

(defgproperty character-body-2d+velocity 'character-body-2d :get
 'character-body-2d+get-velocity :set 'character-body-2d+set-velocity)

(defgproperty character-body-2d+slide-on-ceiling 'character-body-2d :get
 'character-body-2d+is-slide-on-ceiling-enabled :set
 'character-body-2d+set-slide-on-ceiling-enabled)

(defgproperty character-body-2d+max-slides 'character-body-2d :get
 'character-body-2d+get-max-slides :set 'character-body-2d+set-max-slides)

(defgproperty character-body-2d+wall-min-slide-angle 'character-body-2d :get
 'character-body-2d+get-wall-min-slide-angle :set
 'character-body-2d+set-wall-min-slide-angle)

(defgproperty character-body-2d+floor-stop-on-slope 'character-body-2d :get
 'character-body-2d+is-floor-stop-on-slope-enabled :set
 'character-body-2d+set-floor-stop-on-slope-enabled)

(defgproperty character-body-2d+floor-constant-speed 'character-body-2d :get
 'character-body-2d+is-floor-constant-speed-enabled :set
 'character-body-2d+set-floor-constant-speed-enabled)

(defgproperty character-body-2d+floor-block-on-wall 'character-body-2d :get
 'character-body-2d+is-floor-block-on-wall-enabled :set
 'character-body-2d+set-floor-block-on-wall-enabled)

(defgproperty character-body-2d+floor-max-angle 'character-body-2d :get
 'character-body-2d+get-floor-max-angle :set
 'character-body-2d+set-floor-max-angle)

(defgproperty character-body-2d+floor-snap-length 'character-body-2d :get
 'character-body-2d+get-floor-snap-length :set
 'character-body-2d+set-floor-snap-length)

(defgproperty character-body-2d+platform-on-leave 'character-body-2d :get
 'character-body-2d+get-platform-on-leave :set
 'character-body-2d+set-platform-on-leave)

(defgproperty character-body-2d+platform-floor-layers 'character-body-2d :get
 'character-body-2d+get-platform-floor-layers :set
 'character-body-2d+set-platform-floor-layers)

(defgproperty character-body-2d+platform-wall-layers 'character-body-2d :get
 'character-body-2d+get-platform-wall-layers :set
 'character-body-2d+set-platform-wall-layers)

(defgproperty character-body-2d+safe-margin 'character-body-2d :get
 'character-body-2d+get-safe-margin :set 'character-body-2d+set-safe-margin)