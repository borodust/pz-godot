(common-lisp:in-package :%godot)


(defgproperty cpuparticles-3d+emitting 'cpuparticles-3d :get
 'cpuparticles-3d+is-emitting :set 'cpuparticles-3d+set-emitting)

(defgproperty cpuparticles-3d+amount 'cpuparticles-3d :get
 'cpuparticles-3d+get-amount :set 'cpuparticles-3d+set-amount)

(defgproperty cpuparticles-3d+lifetime 'cpuparticles-3d :get
 'cpuparticles-3d+get-lifetime :set 'cpuparticles-3d+set-lifetime)

(defgproperty cpuparticles-3d+one-shot 'cpuparticles-3d :get
 'cpuparticles-3d+get-one-shot :set 'cpuparticles-3d+set-one-shot)

(defgproperty cpuparticles-3d+preprocess 'cpuparticles-3d :get
 'cpuparticles-3d+get-pre-process-time :set
 'cpuparticles-3d+set-pre-process-time)

(defgproperty cpuparticles-3d+speed-scale 'cpuparticles-3d :get
 'cpuparticles-3d+get-speed-scale :set 'cpuparticles-3d+set-speed-scale)

(defgproperty cpuparticles-3d+explosiveness 'cpuparticles-3d :get
 'cpuparticles-3d+get-explosiveness-ratio :set
 'cpuparticles-3d+set-explosiveness-ratio)

(defgproperty cpuparticles-3d+randomness 'cpuparticles-3d :get
 'cpuparticles-3d+get-randomness-ratio :set
 'cpuparticles-3d+set-randomness-ratio)

(defgproperty cpuparticles-3d+use-fixed-seed 'cpuparticles-3d :get
 'cpuparticles-3d+get-use-fixed-seed :set 'cpuparticles-3d+set-use-fixed-seed)

(defgproperty cpuparticles-3d+seed 'cpuparticles-3d :get
 'cpuparticles-3d+get-seed :set 'cpuparticles-3d+set-seed)

(defgproperty cpuparticles-3d+lifetime-randomness 'cpuparticles-3d :get
 'cpuparticles-3d+get-lifetime-randomness :set
 'cpuparticles-3d+set-lifetime-randomness)

(defgproperty cpuparticles-3d+fixed-fps 'cpuparticles-3d :get
 'cpuparticles-3d+get-fixed-fps :set 'cpuparticles-3d+set-fixed-fps)

(defgproperty cpuparticles-3d+fract-delta 'cpuparticles-3d :get
 'cpuparticles-3d+get-fractional-delta :set
 'cpuparticles-3d+set-fractional-delta)

(defgproperty cpuparticles-3d+visibility-aabb 'cpuparticles-3d :get
 'cpuparticles-3d+get-visibility-aabb :set 'cpuparticles-3d+set-visibility-aabb)

(defgproperty cpuparticles-3d+local-coords 'cpuparticles-3d :get
 'cpuparticles-3d+get-use-local-coordinates :set
 'cpuparticles-3d+set-use-local-coordinates)

(defgproperty cpuparticles-3d+draw-order 'cpuparticles-3d :get
 'cpuparticles-3d+get-draw-order :set 'cpuparticles-3d+set-draw-order)

(defgproperty cpuparticles-3d+mesh 'cpuparticles-3d :get
 'cpuparticles-3d+get-mesh :set 'cpuparticles-3d+set-mesh)

(defgproperty cpuparticles-3d+emission-shape 'cpuparticles-3d :get
 'cpuparticles-3d+get-emission-shape :set 'cpuparticles-3d+set-emission-shape)

(defgproperty cpuparticles-3d+emission-sphere-radius 'cpuparticles-3d :get
 'cpuparticles-3d+get-emission-sphere-radius :set
 'cpuparticles-3d+set-emission-sphere-radius)

(defgproperty cpuparticles-3d+emission-box-extents 'cpuparticles-3d :get
 'cpuparticles-3d+get-emission-box-extents :set
 'cpuparticles-3d+set-emission-box-extents)

(defgproperty cpuparticles-3d+emission-points 'cpuparticles-3d :get
 'cpuparticles-3d+get-emission-points :set 'cpuparticles-3d+set-emission-points)

(defgproperty cpuparticles-3d+emission-normals 'cpuparticles-3d :get
 'cpuparticles-3d+get-emission-normals :set
 'cpuparticles-3d+set-emission-normals)

(defgproperty cpuparticles-3d+emission-colors 'cpuparticles-3d :get
 'cpuparticles-3d+get-emission-colors :set 'cpuparticles-3d+set-emission-colors)

(defgproperty cpuparticles-3d+emission-ring-axis 'cpuparticles-3d :get
 'cpuparticles-3d+get-emission-ring-axis :set
 'cpuparticles-3d+set-emission-ring-axis)

(defgproperty cpuparticles-3d+emission-ring-height 'cpuparticles-3d :get
 'cpuparticles-3d+get-emission-ring-height :set
 'cpuparticles-3d+set-emission-ring-height)

(defgproperty cpuparticles-3d+emission-ring-radius 'cpuparticles-3d :get
 'cpuparticles-3d+get-emission-ring-radius :set
 'cpuparticles-3d+set-emission-ring-radius)

(defgproperty cpuparticles-3d+emission-ring-inner-radius 'cpuparticles-3d :get
 'cpuparticles-3d+get-emission-ring-inner-radius :set
 'cpuparticles-3d+set-emission-ring-inner-radius)

(defgproperty cpuparticles-3d+emission-ring-cone-angle 'cpuparticles-3d :get
 'cpuparticles-3d+get-emission-ring-cone-angle :set
 'cpuparticles-3d+set-emission-ring-cone-angle)

(defgproperty cpuparticles-3d+particle-flag-align-y 'cpuparticles-3d :index 0
 :get 'cpuparticles-3d+get-particle-flag :set
 'cpuparticles-3d+set-particle-flag)

(defgproperty cpuparticles-3d+particle-flag-rotate-y 'cpuparticles-3d :index 1
 :get 'cpuparticles-3d+get-particle-flag :set
 'cpuparticles-3d+set-particle-flag)

(defgproperty cpuparticles-3d+particle-flag-disable-z 'cpuparticles-3d :index 2
 :get 'cpuparticles-3d+get-particle-flag :set
 'cpuparticles-3d+set-particle-flag)

(defgproperty cpuparticles-3d+direction 'cpuparticles-3d :get
 'cpuparticles-3d+get-direction :set 'cpuparticles-3d+set-direction)

(defgproperty cpuparticles-3d+spread 'cpuparticles-3d :get
 'cpuparticles-3d+get-spread :set 'cpuparticles-3d+set-spread)

(defgproperty cpuparticles-3d+flatness 'cpuparticles-3d :get
 'cpuparticles-3d+get-flatness :set 'cpuparticles-3d+set-flatness)

(defgproperty cpuparticles-3d+gravity 'cpuparticles-3d :get
 'cpuparticles-3d+get-gravity :set 'cpuparticles-3d+set-gravity)

(defgproperty cpuparticles-3d+initial-velocity-min 'cpuparticles-3d :index 0
 :get 'cpuparticles-3d+get-param-min :set 'cpuparticles-3d+set-param-min)

(defgproperty cpuparticles-3d+initial-velocity-max 'cpuparticles-3d :index 0
 :get 'cpuparticles-3d+get-param-max :set 'cpuparticles-3d+set-param-max)

(defgproperty cpuparticles-3d+angular-velocity-min 'cpuparticles-3d :index 1
 :get 'cpuparticles-3d+get-param-min :set 'cpuparticles-3d+set-param-min)

(defgproperty cpuparticles-3d+angular-velocity-max 'cpuparticles-3d :index 1
 :get 'cpuparticles-3d+get-param-max :set 'cpuparticles-3d+set-param-max)

(defgproperty cpuparticles-3d+angular-velocity-curve 'cpuparticles-3d :index 1
 :get 'cpuparticles-3d+get-param-curve :set 'cpuparticles-3d+set-param-curve)

(defgproperty cpuparticles-3d+orbit-velocity-min 'cpuparticles-3d :index 2 :get
 'cpuparticles-3d+get-param-min :set 'cpuparticles-3d+set-param-min)

(defgproperty cpuparticles-3d+orbit-velocity-max 'cpuparticles-3d :index 2 :get
 'cpuparticles-3d+get-param-max :set 'cpuparticles-3d+set-param-max)

(defgproperty cpuparticles-3d+orbit-velocity-curve 'cpuparticles-3d :index 2
 :get 'cpuparticles-3d+get-param-curve :set 'cpuparticles-3d+set-param-curve)

(defgproperty cpuparticles-3d+linear-accel-min 'cpuparticles-3d :index 3 :get
 'cpuparticles-3d+get-param-min :set 'cpuparticles-3d+set-param-min)

(defgproperty cpuparticles-3d+linear-accel-max 'cpuparticles-3d :index 3 :get
 'cpuparticles-3d+get-param-max :set 'cpuparticles-3d+set-param-max)

(defgproperty cpuparticles-3d+linear-accel-curve 'cpuparticles-3d :index 3 :get
 'cpuparticles-3d+get-param-curve :set 'cpuparticles-3d+set-param-curve)

(defgproperty cpuparticles-3d+radial-accel-min 'cpuparticles-3d :index 4 :get
 'cpuparticles-3d+get-param-min :set 'cpuparticles-3d+set-param-min)

(defgproperty cpuparticles-3d+radial-accel-max 'cpuparticles-3d :index 4 :get
 'cpuparticles-3d+get-param-max :set 'cpuparticles-3d+set-param-max)

(defgproperty cpuparticles-3d+radial-accel-curve 'cpuparticles-3d :index 4 :get
 'cpuparticles-3d+get-param-curve :set 'cpuparticles-3d+set-param-curve)

(defgproperty cpuparticles-3d+tangential-accel-min 'cpuparticles-3d :index 5
 :get 'cpuparticles-3d+get-param-min :set 'cpuparticles-3d+set-param-min)

(defgproperty cpuparticles-3d+tangential-accel-max 'cpuparticles-3d :index 5
 :get 'cpuparticles-3d+get-param-max :set 'cpuparticles-3d+set-param-max)

(defgproperty cpuparticles-3d+tangential-accel-curve 'cpuparticles-3d :index 5
 :get 'cpuparticles-3d+get-param-curve :set 'cpuparticles-3d+set-param-curve)

(defgproperty cpuparticles-3d+damping-min 'cpuparticles-3d :index 6 :get
 'cpuparticles-3d+get-param-min :set 'cpuparticles-3d+set-param-min)

(defgproperty cpuparticles-3d+damping-max 'cpuparticles-3d :index 6 :get
 'cpuparticles-3d+get-param-max :set 'cpuparticles-3d+set-param-max)

(defgproperty cpuparticles-3d+damping-curve 'cpuparticles-3d :index 6 :get
 'cpuparticles-3d+get-param-curve :set 'cpuparticles-3d+set-param-curve)

(defgproperty cpuparticles-3d+angle-min 'cpuparticles-3d :index 7 :get
 'cpuparticles-3d+get-param-min :set 'cpuparticles-3d+set-param-min)

(defgproperty cpuparticles-3d+angle-max 'cpuparticles-3d :index 7 :get
 'cpuparticles-3d+get-param-max :set 'cpuparticles-3d+set-param-max)

(defgproperty cpuparticles-3d+angle-curve 'cpuparticles-3d :index 7 :get
 'cpuparticles-3d+get-param-curve :set 'cpuparticles-3d+set-param-curve)

(defgproperty cpuparticles-3d+scale-amount-min 'cpuparticles-3d :index 8 :get
 'cpuparticles-3d+get-param-min :set 'cpuparticles-3d+set-param-min)

(defgproperty cpuparticles-3d+scale-amount-max 'cpuparticles-3d :index 8 :get
 'cpuparticles-3d+get-param-max :set 'cpuparticles-3d+set-param-max)

(defgproperty cpuparticles-3d+scale-amount-curve 'cpuparticles-3d :index 8 :get
 'cpuparticles-3d+get-param-curve :set 'cpuparticles-3d+set-param-curve)

(defgproperty cpuparticles-3d+split-scale 'cpuparticles-3d :get
 'cpuparticles-3d+get-split-scale :set 'cpuparticles-3d+set-split-scale)

(defgproperty cpuparticles-3d+scale-curve-x 'cpuparticles-3d :get
 'cpuparticles-3d+get-scale-curve-x :set 'cpuparticles-3d+set-scale-curve-x)

(defgproperty cpuparticles-3d+scale-curve-y 'cpuparticles-3d :get
 'cpuparticles-3d+get-scale-curve-y :set 'cpuparticles-3d+set-scale-curve-y)

(defgproperty cpuparticles-3d+scale-curve-z 'cpuparticles-3d :get
 'cpuparticles-3d+get-scale-curve-z :set 'cpuparticles-3d+set-scale-curve-z)

(defgproperty cpuparticles-3d+color 'cpuparticles-3d :get
 'cpuparticles-3d+get-color :set 'cpuparticles-3d+set-color)

(defgproperty cpuparticles-3d+color-ramp 'cpuparticles-3d :get
 'cpuparticles-3d+get-color-ramp :set 'cpuparticles-3d+set-color-ramp)

(defgproperty cpuparticles-3d+color-initial-ramp 'cpuparticles-3d :get
 'cpuparticles-3d+get-color-initial-ramp :set
 'cpuparticles-3d+set-color-initial-ramp)

(defgproperty cpuparticles-3d+hue-variation-min 'cpuparticles-3d :index 9 :get
 'cpuparticles-3d+get-param-min :set 'cpuparticles-3d+set-param-min)

(defgproperty cpuparticles-3d+hue-variation-max 'cpuparticles-3d :index 9 :get
 'cpuparticles-3d+get-param-max :set 'cpuparticles-3d+set-param-max)

(defgproperty cpuparticles-3d+hue-variation-curve 'cpuparticles-3d :index 9
 :get 'cpuparticles-3d+get-param-curve :set 'cpuparticles-3d+set-param-curve)

(defgproperty cpuparticles-3d+anim-speed-min 'cpuparticles-3d :index 10 :get
 'cpuparticles-3d+get-param-min :set 'cpuparticles-3d+set-param-min)

(defgproperty cpuparticles-3d+anim-speed-max 'cpuparticles-3d :index 10 :get
 'cpuparticles-3d+get-param-max :set 'cpuparticles-3d+set-param-max)

(defgproperty cpuparticles-3d+anim-speed-curve 'cpuparticles-3d :index 10 :get
 'cpuparticles-3d+get-param-curve :set 'cpuparticles-3d+set-param-curve)

(defgproperty cpuparticles-3d+anim-offset-min 'cpuparticles-3d :index 11 :get
 'cpuparticles-3d+get-param-min :set 'cpuparticles-3d+set-param-min)

(defgproperty cpuparticles-3d+anim-offset-max 'cpuparticles-3d :index 11 :get
 'cpuparticles-3d+get-param-max :set 'cpuparticles-3d+set-param-max)

(defgproperty cpuparticles-3d+anim-offset-curve 'cpuparticles-3d :index 11 :get
 'cpuparticles-3d+get-param-curve :set 'cpuparticles-3d+set-param-curve)