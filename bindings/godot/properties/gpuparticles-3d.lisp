(common-lisp:in-package :%godot)


(defgproperty gpuparticles-3d+emitting 'gpuparticles-3d :get
 'gpuparticles-3d+is-emitting :set 'gpuparticles-3d+set-emitting)

(defgproperty gpuparticles-3d+amount 'gpuparticles-3d :get
 'gpuparticles-3d+get-amount :set 'gpuparticles-3d+set-amount)

(defgproperty gpuparticles-3d+amount-ratio 'gpuparticles-3d :get
 'gpuparticles-3d+get-amount-ratio :set 'gpuparticles-3d+set-amount-ratio)

(defgproperty gpuparticles-3d+sub-emitter 'gpuparticles-3d :get
 'gpuparticles-3d+get-sub-emitter :set 'gpuparticles-3d+set-sub-emitter)

(defgproperty gpuparticles-3d+lifetime 'gpuparticles-3d :get
 'gpuparticles-3d+get-lifetime :set 'gpuparticles-3d+set-lifetime)

(defgproperty gpuparticles-3d+interp-to-end 'gpuparticles-3d :get
 'gpuparticles-3d+get-interp-to-end :set 'gpuparticles-3d+set-interp-to-end)

(defgproperty gpuparticles-3d+one-shot 'gpuparticles-3d :get
 'gpuparticles-3d+get-one-shot :set 'gpuparticles-3d+set-one-shot)

(defgproperty gpuparticles-3d+preprocess 'gpuparticles-3d :get
 'gpuparticles-3d+get-pre-process-time :set
 'gpuparticles-3d+set-pre-process-time)

(defgproperty gpuparticles-3d+speed-scale 'gpuparticles-3d :get
 'gpuparticles-3d+get-speed-scale :set 'gpuparticles-3d+set-speed-scale)

(defgproperty gpuparticles-3d+explosiveness 'gpuparticles-3d :get
 'gpuparticles-3d+get-explosiveness-ratio :set
 'gpuparticles-3d+set-explosiveness-ratio)

(defgproperty gpuparticles-3d+randomness 'gpuparticles-3d :get
 'gpuparticles-3d+get-randomness-ratio :set
 'gpuparticles-3d+set-randomness-ratio)

(defgproperty gpuparticles-3d+use-fixed-seed 'gpuparticles-3d :get
 'gpuparticles-3d+get-use-fixed-seed :set 'gpuparticles-3d+set-use-fixed-seed)

(defgproperty gpuparticles-3d+seed 'gpuparticles-3d :get
 'gpuparticles-3d+get-seed :set 'gpuparticles-3d+set-seed)

(defgproperty gpuparticles-3d+fixed-fps 'gpuparticles-3d :get
 'gpuparticles-3d+get-fixed-fps :set 'gpuparticles-3d+set-fixed-fps)

(defgproperty gpuparticles-3d+interpolate 'gpuparticles-3d :get
 'gpuparticles-3d+get-interpolate :set 'gpuparticles-3d+set-interpolate)

(defgproperty gpuparticles-3d+fract-delta 'gpuparticles-3d :get
 'gpuparticles-3d+get-fractional-delta :set
 'gpuparticles-3d+set-fractional-delta)

(defgproperty gpuparticles-3d+collision-base-size 'gpuparticles-3d :get
 'gpuparticles-3d+get-collision-base-size :set
 'gpuparticles-3d+set-collision-base-size)

(defgproperty gpuparticles-3d+visibility-aabb 'gpuparticles-3d :get
 'gpuparticles-3d+get-visibility-aabb :set 'gpuparticles-3d+set-visibility-aabb)

(defgproperty gpuparticles-3d+local-coords 'gpuparticles-3d :get
 'gpuparticles-3d+get-use-local-coordinates :set
 'gpuparticles-3d+set-use-local-coordinates)

(defgproperty gpuparticles-3d+draw-order 'gpuparticles-3d :get
 'gpuparticles-3d+get-draw-order :set 'gpuparticles-3d+set-draw-order)

(defgproperty gpuparticles-3d+transform-align 'gpuparticles-3d :get
 'gpuparticles-3d+get-transform-align :set 'gpuparticles-3d+set-transform-align)

(defgproperty gpuparticles-3d+transform-align-axis 'gpuparticles-3d :get
 'gpuparticles-3d+get-transform-align-axis :set
 'gpuparticles-3d+set-transform-align-axis)

(defgproperty gpuparticles-3d+transform-align-channel-filter 'gpuparticles-3d
 :get 'gpuparticles-3d+get-transform-align-channel-filter :set
 'gpuparticles-3d+set-transform-align-channel-filter)

(defgproperty gpuparticles-3d+trail-enabled 'gpuparticles-3d :get
 'gpuparticles-3d+is-trail-enabled :set 'gpuparticles-3d+set-trail-enabled)

(defgproperty gpuparticles-3d+trail-lifetime 'gpuparticles-3d :get
 'gpuparticles-3d+get-trail-lifetime :set 'gpuparticles-3d+set-trail-lifetime)

(defgproperty gpuparticles-3d+process-material 'gpuparticles-3d :get
 'gpuparticles-3d+get-process-material :set
 'gpuparticles-3d+set-process-material)

(defgproperty gpuparticles-3d+draw-passes 'gpuparticles-3d :get
 'gpuparticles-3d+get-draw-passes :set 'gpuparticles-3d+set-draw-passes)

(defgproperty gpuparticles-3d+draw-pass-1 'gpuparticles-3d :index 0 :get
 'gpuparticles-3d+get-draw-pass-mesh :set 'gpuparticles-3d+set-draw-pass-mesh)

(defgproperty gpuparticles-3d+draw-pass-2 'gpuparticles-3d :index 1 :get
 'gpuparticles-3d+get-draw-pass-mesh :set 'gpuparticles-3d+set-draw-pass-mesh)

(defgproperty gpuparticles-3d+draw-pass-3 'gpuparticles-3d :index 2 :get
 'gpuparticles-3d+get-draw-pass-mesh :set 'gpuparticles-3d+set-draw-pass-mesh)

(defgproperty gpuparticles-3d+draw-pass-4 'gpuparticles-3d :index 3 :get
 'gpuparticles-3d+get-draw-pass-mesh :set 'gpuparticles-3d+set-draw-pass-mesh)

(defgproperty gpuparticles-3d+draw-skin 'gpuparticles-3d :get
 'gpuparticles-3d+get-skin :set 'gpuparticles-3d+set-skin)