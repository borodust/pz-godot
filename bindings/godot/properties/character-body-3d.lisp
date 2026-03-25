(common-lisp:in-package :%godot)


(defgproperty character-body-3d+motion-mode 'character-body-3d :get
 'character-body-3d+get-motion-mode :set 'character-body-3d+set-motion-mode)

(defgproperty character-body-3d+up-direction 'character-body-3d :get
 'character-body-3d+get-up-direction :set 'character-body-3d+set-up-direction)

(defgproperty character-body-3d+slide-on-ceiling 'character-body-3d :get
 'character-body-3d+is-slide-on-ceiling-enabled :set
 'character-body-3d+set-slide-on-ceiling-enabled)

(defgproperty character-body-3d+velocity 'character-body-3d :get
 'character-body-3d+get-velocity :set 'character-body-3d+set-velocity)

(defgproperty character-body-3d+max-slides 'character-body-3d :get
 'character-body-3d+get-max-slides :set 'character-body-3d+set-max-slides)

(defgproperty character-body-3d+wall-min-slide-angle 'character-body-3d :get
 'character-body-3d+get-wall-min-slide-angle :set
 'character-body-3d+set-wall-min-slide-angle)

(defgproperty character-body-3d+floor-stop-on-slope 'character-body-3d :get
 'character-body-3d+is-floor-stop-on-slope-enabled :set
 'character-body-3d+set-floor-stop-on-slope-enabled)

(defgproperty character-body-3d+floor-constant-speed 'character-body-3d :get
 'character-body-3d+is-floor-constant-speed-enabled :set
 'character-body-3d+set-floor-constant-speed-enabled)

(defgproperty character-body-3d+floor-block-on-wall 'character-body-3d :get
 'character-body-3d+is-floor-block-on-wall-enabled :set
 'character-body-3d+set-floor-block-on-wall-enabled)

(defgproperty character-body-3d+floor-max-angle 'character-body-3d :get
 'character-body-3d+get-floor-max-angle :set
 'character-body-3d+set-floor-max-angle)

(defgproperty character-body-3d+floor-snap-length 'character-body-3d :get
 'character-body-3d+get-floor-snap-length :set
 'character-body-3d+set-floor-snap-length)

(defgproperty character-body-3d+platform-on-leave 'character-body-3d :get
 'character-body-3d+get-platform-on-leave :set
 'character-body-3d+set-platform-on-leave)

(defgproperty character-body-3d+platform-floor-layers 'character-body-3d :get
 'character-body-3d+get-platform-floor-layers :set
 'character-body-3d+set-platform-floor-layers)

(defgproperty character-body-3d+platform-wall-layers 'character-body-3d :get
 'character-body-3d+get-platform-wall-layers :set
 'character-body-3d+set-platform-wall-layers)

(defgproperty character-body-3d+safe-margin 'character-body-3d :get
 'character-body-3d+get-safe-margin :set 'character-body-3d+set-safe-margin)