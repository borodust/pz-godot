(common-lisp:in-package :%godot)


(defgproperty cpuparticles-2d+emitting 'cpuparticles-2d :get
 'cpuparticles-2d+is-emitting :set 'cpuparticles-2d+set-emitting)

(defgproperty cpuparticles-2d+amount 'cpuparticles-2d :get
 'cpuparticles-2d+get-amount :set 'cpuparticles-2d+set-amount)

(defgproperty cpuparticles-2d+texture 'cpuparticles-2d :get
 'cpuparticles-2d+get-texture :set 'cpuparticles-2d+set-texture)

(defgproperty cpuparticles-2d+lifetime 'cpuparticles-2d :get
 'cpuparticles-2d+get-lifetime :set 'cpuparticles-2d+set-lifetime)

(defgproperty cpuparticles-2d+one-shot 'cpuparticles-2d :get
 'cpuparticles-2d+get-one-shot :set 'cpuparticles-2d+set-one-shot)

(defgproperty cpuparticles-2d+preprocess 'cpuparticles-2d :get
 'cpuparticles-2d+get-pre-process-time :set
 'cpuparticles-2d+set-pre-process-time)

(defgproperty cpuparticles-2d+speed-scale 'cpuparticles-2d :get
 'cpuparticles-2d+get-speed-scale :set 'cpuparticles-2d+set-speed-scale)

(defgproperty cpuparticles-2d+explosiveness 'cpuparticles-2d :get
 'cpuparticles-2d+get-explosiveness-ratio :set
 'cpuparticles-2d+set-explosiveness-ratio)

(defgproperty cpuparticles-2d+randomness 'cpuparticles-2d :get
 'cpuparticles-2d+get-randomness-ratio :set
 'cpuparticles-2d+set-randomness-ratio)

(defgproperty cpuparticles-2d+use-fixed-seed 'cpuparticles-2d :get
 'cpuparticles-2d+get-use-fixed-seed :set 'cpuparticles-2d+set-use-fixed-seed)

(defgproperty cpuparticles-2d+seed 'cpuparticles-2d :get
 'cpuparticles-2d+get-seed :set 'cpuparticles-2d+set-seed)

(defgproperty cpuparticles-2d+lifetime-randomness 'cpuparticles-2d :get
 'cpuparticles-2d+get-lifetime-randomness :set
 'cpuparticles-2d+set-lifetime-randomness)

(defgproperty cpuparticles-2d+fixed-fps 'cpuparticles-2d :get
 'cpuparticles-2d+get-fixed-fps :set 'cpuparticles-2d+set-fixed-fps)

(defgproperty cpuparticles-2d+fract-delta 'cpuparticles-2d :get
 'cpuparticles-2d+get-fractional-delta :set
 'cpuparticles-2d+set-fractional-delta)

(defgproperty cpuparticles-2d+local-coords 'cpuparticles-2d :get
 'cpuparticles-2d+get-use-local-coordinates :set
 'cpuparticles-2d+set-use-local-coordinates)

(defgproperty cpuparticles-2d+draw-order 'cpuparticles-2d :get
 'cpuparticles-2d+get-draw-order :set 'cpuparticles-2d+set-draw-order)

(defgproperty cpuparticles-2d+emission-shape 'cpuparticles-2d :get
 'cpuparticles-2d+get-emission-shape :set 'cpuparticles-2d+set-emission-shape)

(defgproperty cpuparticles-2d+emission-sphere-radius 'cpuparticles-2d :get
 'cpuparticles-2d+get-emission-sphere-radius :set
 'cpuparticles-2d+set-emission-sphere-radius)

(defgproperty cpuparticles-2d+emission-rect-extents 'cpuparticles-2d :get
 'cpuparticles-2d+get-emission-rect-extents :set
 'cpuparticles-2d+set-emission-rect-extents)

(defgproperty cpuparticles-2d+emission-points 'cpuparticles-2d :get
 'cpuparticles-2d+get-emission-points :set 'cpuparticles-2d+set-emission-points)

(defgproperty cpuparticles-2d+emission-normals 'cpuparticles-2d :get
 'cpuparticles-2d+get-emission-normals :set
 'cpuparticles-2d+set-emission-normals)

(defgproperty cpuparticles-2d+emission-colors 'cpuparticles-2d :get
 'cpuparticles-2d+get-emission-colors :set 'cpuparticles-2d+set-emission-colors)

(defgproperty cpuparticles-2d+emission-ring-inner-radius 'cpuparticles-2d :get
 'cpuparticles-2d+get-emission-ring-inner-radius :set
 'cpuparticles-2d+set-emission-ring-inner-radius)

(defgproperty cpuparticles-2d+emission-ring-radius 'cpuparticles-2d :get
 'cpuparticles-2d+get-emission-ring-radius :set
 'cpuparticles-2d+set-emission-ring-radius)

(defgproperty cpuparticles-2d+particle-flag-align-y 'cpuparticles-2d :index 0
 :get 'cpuparticles-2d+get-particle-flag :set
 'cpuparticles-2d+set-particle-flag)

(defgproperty cpuparticles-2d+direction 'cpuparticles-2d :get
 'cpuparticles-2d+get-direction :set 'cpuparticles-2d+set-direction)

(defgproperty cpuparticles-2d+spread 'cpuparticles-2d :get
 'cpuparticles-2d+get-spread :set 'cpuparticles-2d+set-spread)

(defgproperty cpuparticles-2d+gravity 'cpuparticles-2d :get
 'cpuparticles-2d+get-gravity :set 'cpuparticles-2d+set-gravity)

(defgproperty cpuparticles-2d+initial-velocity-min 'cpuparticles-2d :index 0
 :get 'cpuparticles-2d+get-param-min :set 'cpuparticles-2d+set-param-min)

(defgproperty cpuparticles-2d+initial-velocity-max 'cpuparticles-2d :index 0
 :get 'cpuparticles-2d+get-param-max :set 'cpuparticles-2d+set-param-max)

(defgproperty cpuparticles-2d+angular-velocity-min 'cpuparticles-2d :index 1
 :get 'cpuparticles-2d+get-param-min :set 'cpuparticles-2d+set-param-min)

(defgproperty cpuparticles-2d+angular-velocity-max 'cpuparticles-2d :index 1
 :get 'cpuparticles-2d+get-param-max :set 'cpuparticles-2d+set-param-max)

(defgproperty cpuparticles-2d+angular-velocity-curve 'cpuparticles-2d :index 1
 :get 'cpuparticles-2d+get-param-curve :set 'cpuparticles-2d+set-param-curve)

(defgproperty cpuparticles-2d+orbit-velocity-min 'cpuparticles-2d :index 2 :get
 'cpuparticles-2d+get-param-min :set 'cpuparticles-2d+set-param-min)

(defgproperty cpuparticles-2d+orbit-velocity-max 'cpuparticles-2d :index 2 :get
 'cpuparticles-2d+get-param-max :set 'cpuparticles-2d+set-param-max)

(defgproperty cpuparticles-2d+orbit-velocity-curve 'cpuparticles-2d :index 2
 :get 'cpuparticles-2d+get-param-curve :set 'cpuparticles-2d+set-param-curve)

(defgproperty cpuparticles-2d+linear-accel-min 'cpuparticles-2d :index 3 :get
 'cpuparticles-2d+get-param-min :set 'cpuparticles-2d+set-param-min)

(defgproperty cpuparticles-2d+linear-accel-max 'cpuparticles-2d :index 3 :get
 'cpuparticles-2d+get-param-max :set 'cpuparticles-2d+set-param-max)

(defgproperty cpuparticles-2d+linear-accel-curve 'cpuparticles-2d :index 3 :get
 'cpuparticles-2d+get-param-curve :set 'cpuparticles-2d+set-param-curve)

(defgproperty cpuparticles-2d+radial-accel-min 'cpuparticles-2d :index 4 :get
 'cpuparticles-2d+get-param-min :set 'cpuparticles-2d+set-param-min)

(defgproperty cpuparticles-2d+radial-accel-max 'cpuparticles-2d :index 4 :get
 'cpuparticles-2d+get-param-max :set 'cpuparticles-2d+set-param-max)

(defgproperty cpuparticles-2d+radial-accel-curve 'cpuparticles-2d :index 4 :get
 'cpuparticles-2d+get-param-curve :set 'cpuparticles-2d+set-param-curve)

(defgproperty cpuparticles-2d+tangential-accel-min 'cpuparticles-2d :index 5
 :get 'cpuparticles-2d+get-param-min :set 'cpuparticles-2d+set-param-min)

(defgproperty cpuparticles-2d+tangential-accel-max 'cpuparticles-2d :index 5
 :get 'cpuparticles-2d+get-param-max :set 'cpuparticles-2d+set-param-max)

(defgproperty cpuparticles-2d+tangential-accel-curve 'cpuparticles-2d :index 5
 :get 'cpuparticles-2d+get-param-curve :set 'cpuparticles-2d+set-param-curve)

(defgproperty cpuparticles-2d+damping-min 'cpuparticles-2d :index 6 :get
 'cpuparticles-2d+get-param-min :set 'cpuparticles-2d+set-param-min)

(defgproperty cpuparticles-2d+damping-max 'cpuparticles-2d :index 6 :get
 'cpuparticles-2d+get-param-max :set 'cpuparticles-2d+set-param-max)

(defgproperty cpuparticles-2d+damping-curve 'cpuparticles-2d :index 6 :get
 'cpuparticles-2d+get-param-curve :set 'cpuparticles-2d+set-param-curve)

(defgproperty cpuparticles-2d+angle-min 'cpuparticles-2d :index 7 :get
 'cpuparticles-2d+get-param-min :set 'cpuparticles-2d+set-param-min)

(defgproperty cpuparticles-2d+angle-max 'cpuparticles-2d :index 7 :get
 'cpuparticles-2d+get-param-max :set 'cpuparticles-2d+set-param-max)

(defgproperty cpuparticles-2d+angle-curve 'cpuparticles-2d :index 7 :get
 'cpuparticles-2d+get-param-curve :set 'cpuparticles-2d+set-param-curve)

(defgproperty cpuparticles-2d+scale-amount-min 'cpuparticles-2d :index 8 :get
 'cpuparticles-2d+get-param-min :set 'cpuparticles-2d+set-param-min)

(defgproperty cpuparticles-2d+scale-amount-max 'cpuparticles-2d :index 8 :get
 'cpuparticles-2d+get-param-max :set 'cpuparticles-2d+set-param-max)

(defgproperty cpuparticles-2d+scale-amount-curve 'cpuparticles-2d :index 8 :get
 'cpuparticles-2d+get-param-curve :set 'cpuparticles-2d+set-param-curve)

(defgproperty cpuparticles-2d+split-scale 'cpuparticles-2d :get
 'cpuparticles-2d+get-split-scale :set 'cpuparticles-2d+set-split-scale)

(defgproperty cpuparticles-2d+scale-curve-x 'cpuparticles-2d :get
 'cpuparticles-2d+get-scale-curve-x :set 'cpuparticles-2d+set-scale-curve-x)

(defgproperty cpuparticles-2d+scale-curve-y 'cpuparticles-2d :get
 'cpuparticles-2d+get-scale-curve-y :set 'cpuparticles-2d+set-scale-curve-y)

(defgproperty cpuparticles-2d+color 'cpuparticles-2d :get
 'cpuparticles-2d+get-color :set 'cpuparticles-2d+set-color)

(defgproperty cpuparticles-2d+color-ramp 'cpuparticles-2d :get
 'cpuparticles-2d+get-color-ramp :set 'cpuparticles-2d+set-color-ramp)

(defgproperty cpuparticles-2d+color-initial-ramp 'cpuparticles-2d :get
 'cpuparticles-2d+get-color-initial-ramp :set
 'cpuparticles-2d+set-color-initial-ramp)

(defgproperty cpuparticles-2d+hue-variation-min 'cpuparticles-2d :index 9 :get
 'cpuparticles-2d+get-param-min :set 'cpuparticles-2d+set-param-min)

(defgproperty cpuparticles-2d+hue-variation-max 'cpuparticles-2d :index 9 :get
 'cpuparticles-2d+get-param-max :set 'cpuparticles-2d+set-param-max)

(defgproperty cpuparticles-2d+hue-variation-curve 'cpuparticles-2d :index 9
 :get 'cpuparticles-2d+get-param-curve :set 'cpuparticles-2d+set-param-curve)

(defgproperty cpuparticles-2d+anim-speed-min 'cpuparticles-2d :index 10 :get
 'cpuparticles-2d+get-param-min :set 'cpuparticles-2d+set-param-min)

(defgproperty cpuparticles-2d+anim-speed-max 'cpuparticles-2d :index 10 :get
 'cpuparticles-2d+get-param-max :set 'cpuparticles-2d+set-param-max)

(defgproperty cpuparticles-2d+anim-speed-curve 'cpuparticles-2d :index 10 :get
 'cpuparticles-2d+get-param-curve :set 'cpuparticles-2d+set-param-curve)

(defgproperty cpuparticles-2d+anim-offset-min 'cpuparticles-2d :index 11 :get
 'cpuparticles-2d+get-param-min :set 'cpuparticles-2d+set-param-min)

(defgproperty cpuparticles-2d+anim-offset-max 'cpuparticles-2d :index 11 :get
 'cpuparticles-2d+get-param-max :set 'cpuparticles-2d+set-param-max)

(defgproperty cpuparticles-2d+anim-offset-curve 'cpuparticles-2d :index 11 :get
 'cpuparticles-2d+get-param-curve :set 'cpuparticles-2d+set-param-curve)