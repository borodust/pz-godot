(common-lisp:in-package :%godot)


(defgproperty particle-process-material+lifetime-randomness
 'particle-process-material :get
 'particle-process-material+get-lifetime-randomness :set
 'particle-process-material+set-lifetime-randomness)

(defgproperty particle-process-material+particle-flag-align-y
 'particle-process-material :index 0 :get
 'particle-process-material+get-particle-flag :set
 'particle-process-material+set-particle-flag)

(defgproperty particle-process-material+particle-flag-rotate-y
 'particle-process-material :index 1 :get
 'particle-process-material+get-particle-flag :set
 'particle-process-material+set-particle-flag)

(defgproperty particle-process-material+particle-flag-disable-z
 'particle-process-material :index 2 :get
 'particle-process-material+get-particle-flag :set
 'particle-process-material+set-particle-flag)

(defgproperty particle-process-material+particle-flag-damping-as-friction
 'particle-process-material :index 3 :get
 'particle-process-material+get-particle-flag :set
 'particle-process-material+set-particle-flag)

(defgproperty particle-process-material+particle-flag-inherit-emitter-scale
 'particle-process-material :index 4 :get
 'particle-process-material+get-particle-flag :set
 'particle-process-material+set-particle-flag)

(defgproperty particle-process-material+emission-shape-offset
 'particle-process-material :get
 'particle-process-material+get-emission-shape-offset :set
 'particle-process-material+set-emission-shape-offset)

(defgproperty particle-process-material+emission-shape-scale
 'particle-process-material :get
 'particle-process-material+get-emission-shape-scale :set
 'particle-process-material+set-emission-shape-scale)

(defgproperty particle-process-material+emission-shape
 'particle-process-material :get 'particle-process-material+get-emission-shape
 :set 'particle-process-material+set-emission-shape)

(defgproperty particle-process-material+emission-sphere-radius
 'particle-process-material :get
 'particle-process-material+get-emission-sphere-radius :set
 'particle-process-material+set-emission-sphere-radius)

(defgproperty particle-process-material+emission-box-extents
 'particle-process-material :get
 'particle-process-material+get-emission-box-extents :set
 'particle-process-material+set-emission-box-extents)

(defgproperty particle-process-material+emission-point-texture
 'particle-process-material :get
 'particle-process-material+get-emission-point-texture :set
 'particle-process-material+set-emission-point-texture)

(defgproperty particle-process-material+emission-normal-texture
 'particle-process-material :get
 'particle-process-material+get-emission-normal-texture :set
 'particle-process-material+set-emission-normal-texture)

(defgproperty particle-process-material+emission-color-texture
 'particle-process-material :get
 'particle-process-material+get-emission-color-texture :set
 'particle-process-material+set-emission-color-texture)

(defgproperty particle-process-material+emission-point-count
 'particle-process-material :get
 'particle-process-material+get-emission-point-count :set
 'particle-process-material+set-emission-point-count)

(defgproperty particle-process-material+emission-ring-axis
 'particle-process-material :get
 'particle-process-material+get-emission-ring-axis :set
 'particle-process-material+set-emission-ring-axis)

(defgproperty particle-process-material+emission-ring-height
 'particle-process-material :get
 'particle-process-material+get-emission-ring-height :set
 'particle-process-material+set-emission-ring-height)

(defgproperty particle-process-material+emission-ring-radius
 'particle-process-material :get
 'particle-process-material+get-emission-ring-radius :set
 'particle-process-material+set-emission-ring-radius)

(defgproperty particle-process-material+emission-ring-inner-radius
 'particle-process-material :get
 'particle-process-material+get-emission-ring-inner-radius :set
 'particle-process-material+set-emission-ring-inner-radius)

(defgproperty particle-process-material+emission-ring-cone-angle
 'particle-process-material :get
 'particle-process-material+get-emission-ring-cone-angle :set
 'particle-process-material+set-emission-ring-cone-angle)

(defgproperty particle-process-material+angle 'particle-process-material :index
 7 :get 'particle-process-material+get-param :set
 'particle-process-material+set-param)

(defgproperty particle-process-material+angle-min 'particle-process-material
 :index 7 :get 'particle-process-material+get-param-min :set
 'particle-process-material+set-param-min)

(defgproperty particle-process-material+angle-max 'particle-process-material
 :index 7 :get 'particle-process-material+get-param-max :set
 'particle-process-material+set-param-max)

(defgproperty particle-process-material+angle-curve 'particle-process-material
 :index 7 :get 'particle-process-material+get-param-texture :set
 'particle-process-material+set-param-texture)

(defgproperty particle-process-material+use-rotation-3d
 'particle-process-material :get
 'particle-process-material+is-using-rotation-3d :set
 'particle-process-material+set-use-rotation-3d)

(defgproperty particle-process-material+rotation-3d-min
 'particle-process-material :get 'particle-process-material+get-rotation-3d-min
 :set 'particle-process-material+set-rotation-3d-min)

(defgproperty particle-process-material+rotation-3d-max
 'particle-process-material :get 'particle-process-material+get-rotation-3d-max
 :set 'particle-process-material+set-rotation-3d-max)

(defgproperty particle-process-material+inherit-velocity-ratio
 'particle-process-material :get
 'particle-process-material+get-inherit-velocity-ratio :set
 'particle-process-material+set-inherit-velocity-ratio)

(defgproperty particle-process-material+velocity-pivot
 'particle-process-material :get 'particle-process-material+get-velocity-pivot
 :set 'particle-process-material+set-velocity-pivot)

(defgproperty particle-process-material+direction 'particle-process-material
 :get 'particle-process-material+get-direction :set
 'particle-process-material+set-direction)

(defgproperty particle-process-material+spread 'particle-process-material :get
 'particle-process-material+get-spread :set
 'particle-process-material+set-spread)

(defgproperty particle-process-material+flatness 'particle-process-material
 :get 'particle-process-material+get-flatness :set
 'particle-process-material+set-flatness)

(defgproperty particle-process-material+initial-velocity
 'particle-process-material :index 0 :get 'particle-process-material+get-param
 :set 'particle-process-material+set-param)

(defgproperty particle-process-material+initial-velocity-min
 'particle-process-material :index 0 :get
 'particle-process-material+get-param-min :set
 'particle-process-material+set-param-min)

(defgproperty particle-process-material+initial-velocity-max
 'particle-process-material :index 0 :get
 'particle-process-material+get-param-max :set
 'particle-process-material+set-param-max)

(defgproperty particle-process-material+angular-velocity
 'particle-process-material :index 1 :get 'particle-process-material+get-param
 :set 'particle-process-material+set-param)

(defgproperty particle-process-material+angular-velocity-min
 'particle-process-material :index 1 :get
 'particle-process-material+get-param-min :set
 'particle-process-material+set-param-min)

(defgproperty particle-process-material+angular-velocity-max
 'particle-process-material :index 1 :get
 'particle-process-material+get-param-max :set
 'particle-process-material+set-param-max)

(defgproperty particle-process-material+angular-velocity-curve
 'particle-process-material :index 1 :get
 'particle-process-material+get-param-texture :set
 'particle-process-material+set-param-texture)

(defgproperty particle-process-material+directional-velocity
 'particle-process-material :index 16 :get 'particle-process-material+get-param
 :set 'particle-process-material+set-param)

(defgproperty particle-process-material+directional-velocity-min
 'particle-process-material :index 16 :get
 'particle-process-material+get-param-min :set
 'particle-process-material+set-param-min)

(defgproperty particle-process-material+directional-velocity-max
 'particle-process-material :index 16 :get
 'particle-process-material+get-param-max :set
 'particle-process-material+set-param-max)

(defgproperty particle-process-material+directional-velocity-curve
 'particle-process-material :index 16 :get
 'particle-process-material+get-param-texture :set
 'particle-process-material+set-param-texture)

(defgproperty particle-process-material+orbit-velocity
 'particle-process-material :index 2 :get 'particle-process-material+get-param
 :set 'particle-process-material+set-param)

(defgproperty particle-process-material+orbit-velocity-min
 'particle-process-material :index 2 :get
 'particle-process-material+get-param-min :set
 'particle-process-material+set-param-min)

(defgproperty particle-process-material+orbit-velocity-max
 'particle-process-material :index 2 :get
 'particle-process-material+get-param-max :set
 'particle-process-material+set-param-max)

(defgproperty particle-process-material+orbit-velocity-curve
 'particle-process-material :index 2 :get
 'particle-process-material+get-param-texture :set
 'particle-process-material+set-param-texture)

(defgproperty particle-process-material+radial-velocity
 'particle-process-material :index 15 :get 'particle-process-material+get-param
 :set 'particle-process-material+set-param)

(defgproperty particle-process-material+radial-velocity-min
 'particle-process-material :index 15 :get
 'particle-process-material+get-param-min :set
 'particle-process-material+set-param-min)

(defgproperty particle-process-material+radial-velocity-max
 'particle-process-material :index 15 :get
 'particle-process-material+get-param-max :set
 'particle-process-material+set-param-max)

(defgproperty particle-process-material+radial-velocity-curve
 'particle-process-material :index 15 :get
 'particle-process-material+get-param-texture :set
 'particle-process-material+set-param-texture)

(defgproperty particle-process-material+velocity-limit-curve
 'particle-process-material :get
 'particle-process-material+get-velocity-limit-curve :set
 'particle-process-material+set-velocity-limit-curve)

(defgproperty particle-process-material+use-rotation-velocity-3d
 'particle-process-material :get
 'particle-process-material+is-using-rotation-velocity-3d :set
 'particle-process-material+set-using-rotation-velocity-3d)

(defgproperty particle-process-material+rotation-velocity-3d-min
 'particle-process-material :get
 'particle-process-material+get-rotation-velocity-3d-min :set
 'particle-process-material+set-rotation-velocity-3d-min)

(defgproperty particle-process-material+rotation-velocity-3d-max
 'particle-process-material :get
 'particle-process-material+get-rotation-velocity-3d-max :set
 'particle-process-material+set-rotation-velocity-3d-max)

(defgproperty particle-process-material+rotation-velocity-3d-curve
 'particle-process-material :get
 'particle-process-material+get-rotation-velocity-3d-curve :set
 'particle-process-material+set-rotation-velocity-3d-curve)

(defgproperty particle-process-material+gravity 'particle-process-material :get
 'particle-process-material+get-gravity :set
 'particle-process-material+set-gravity)

(defgproperty particle-process-material+linear-accel 'particle-process-material
 :index 3 :get 'particle-process-material+get-param :set
 'particle-process-material+set-param)

(defgproperty particle-process-material+linear-accel-min
 'particle-process-material :index 3 :get
 'particle-process-material+get-param-min :set
 'particle-process-material+set-param-min)

(defgproperty particle-process-material+linear-accel-max
 'particle-process-material :index 3 :get
 'particle-process-material+get-param-max :set
 'particle-process-material+set-param-max)

(defgproperty particle-process-material+linear-accel-curve
 'particle-process-material :index 3 :get
 'particle-process-material+get-param-texture :set
 'particle-process-material+set-param-texture)

(defgproperty particle-process-material+radial-accel 'particle-process-material
 :index 4 :get 'particle-process-material+get-param :set
 'particle-process-material+set-param)

(defgproperty particle-process-material+radial-accel-min
 'particle-process-material :index 4 :get
 'particle-process-material+get-param-min :set
 'particle-process-material+set-param-min)

(defgproperty particle-process-material+radial-accel-max
 'particle-process-material :index 4 :get
 'particle-process-material+get-param-max :set
 'particle-process-material+set-param-max)

(defgproperty particle-process-material+radial-accel-curve
 'particle-process-material :index 4 :get
 'particle-process-material+get-param-texture :set
 'particle-process-material+set-param-texture)

(defgproperty particle-process-material+tangential-accel
 'particle-process-material :index 5 :get 'particle-process-material+get-param
 :set 'particle-process-material+set-param)

(defgproperty particle-process-material+tangential-accel-min
 'particle-process-material :index 5 :get
 'particle-process-material+get-param-min :set
 'particle-process-material+set-param-min)

(defgproperty particle-process-material+tangential-accel-max
 'particle-process-material :index 5 :get
 'particle-process-material+get-param-max :set
 'particle-process-material+set-param-max)

(defgproperty particle-process-material+tangential-accel-curve
 'particle-process-material :index 5 :get
 'particle-process-material+get-param-texture :set
 'particle-process-material+set-param-texture)

(defgproperty particle-process-material+damping 'particle-process-material
 :index 6 :get 'particle-process-material+get-param :set
 'particle-process-material+set-param)

(defgproperty particle-process-material+damping-min 'particle-process-material
 :index 6 :get 'particle-process-material+get-param-min :set
 'particle-process-material+set-param-min)

(defgproperty particle-process-material+damping-max 'particle-process-material
 :index 6 :get 'particle-process-material+get-param-max :set
 'particle-process-material+set-param-max)

(defgproperty particle-process-material+damping-curve
 'particle-process-material :index 6 :get
 'particle-process-material+get-param-texture :set
 'particle-process-material+set-param-texture)

(defgproperty particle-process-material+attractor-interaction-enabled
 'particle-process-material :get
 'particle-process-material+is-attractor-interaction-enabled :set
 'particle-process-material+set-attractor-interaction-enabled)

(defgproperty particle-process-material+use-scale-3d 'particle-process-material
 :get 'particle-process-material+is-using-scale-3d :set
 'particle-process-material+set-use-scale-3d)

(defgproperty particle-process-material+scale-3d-min 'particle-process-material
 :get 'particle-process-material+get-scale-3d-min :set
 'particle-process-material+set-scale-3d-min)

(defgproperty particle-process-material+scale-3d-max 'particle-process-material
 :get 'particle-process-material+get-scale-3d-max :set
 'particle-process-material+set-scale-3d-max)

(defgproperty particle-process-material+scale 'particle-process-material :index
 8 :get 'particle-process-material+get-param :set
 'particle-process-material+set-param)

(defgproperty particle-process-material+scale-min 'particle-process-material
 :index 8 :get 'particle-process-material+get-param-min :set
 'particle-process-material+set-param-min)

(defgproperty particle-process-material+scale-max 'particle-process-material
 :index 8 :get 'particle-process-material+get-param-max :set
 'particle-process-material+set-param-max)

(defgproperty particle-process-material+scale-curve 'particle-process-material
 :index 8 :get 'particle-process-material+get-param-texture :set
 'particle-process-material+set-param-texture)

(defgproperty particle-process-material+scale-over-velocity
 'particle-process-material :index 17 :get 'particle-process-material+get-param
 :set 'particle-process-material+set-param)

(defgproperty particle-process-material+scale-over-velocity-min
 'particle-process-material :index 17 :get
 'particle-process-material+get-param-min :set
 'particle-process-material+set-param-min)

(defgproperty particle-process-material+scale-over-velocity-max
 'particle-process-material :index 17 :get
 'particle-process-material+get-param-max :set
 'particle-process-material+set-param-max)

(defgproperty particle-process-material+scale-over-velocity-curve
 'particle-process-material :index 17 :get
 'particle-process-material+get-param-texture :set
 'particle-process-material+set-param-texture)

(defgproperty particle-process-material+color 'particle-process-material :get
 'particle-process-material+get-color :set 'particle-process-material+set-color)

(defgproperty particle-process-material+color-ramp 'particle-process-material
 :get 'particle-process-material+get-color-ramp :set
 'particle-process-material+set-color-ramp)

(defgproperty particle-process-material+color-initial-ramp
 'particle-process-material :get
 'particle-process-material+get-color-initial-ramp :set
 'particle-process-material+set-color-initial-ramp)

(defgproperty particle-process-material+alpha-curve 'particle-process-material
 :get 'particle-process-material+get-alpha-curve :set
 'particle-process-material+set-alpha-curve)

(defgproperty particle-process-material+emission-curve
 'particle-process-material :get 'particle-process-material+get-emission-curve
 :set 'particle-process-material+set-emission-curve)

(defgproperty particle-process-material+hue-variation
 'particle-process-material :index 9 :get 'particle-process-material+get-param
 :set 'particle-process-material+set-param)

(defgproperty particle-process-material+hue-variation-min
 'particle-process-material :index 9 :get
 'particle-process-material+get-param-min :set
 'particle-process-material+set-param-min)

(defgproperty particle-process-material+hue-variation-max
 'particle-process-material :index 9 :get
 'particle-process-material+get-param-max :set
 'particle-process-material+set-param-max)

(defgproperty particle-process-material+hue-variation-curve
 'particle-process-material :index 9 :get
 'particle-process-material+get-param-texture :set
 'particle-process-material+set-param-texture)

(defgproperty particle-process-material+anim-speed 'particle-process-material
 :index 10 :get 'particle-process-material+get-param :set
 'particle-process-material+set-param)

(defgproperty particle-process-material+anim-speed-min
 'particle-process-material :index 10 :get
 'particle-process-material+get-param-min :set
 'particle-process-material+set-param-min)

(defgproperty particle-process-material+anim-speed-max
 'particle-process-material :index 10 :get
 'particle-process-material+get-param-max :set
 'particle-process-material+set-param-max)

(defgproperty particle-process-material+anim-speed-curve
 'particle-process-material :index 10 :get
 'particle-process-material+get-param-texture :set
 'particle-process-material+set-param-texture)

(defgproperty particle-process-material+anim-offset 'particle-process-material
 :index 11 :get 'particle-process-material+get-param :set
 'particle-process-material+set-param)

(defgproperty particle-process-material+anim-offset-min
 'particle-process-material :index 11 :get
 'particle-process-material+get-param-min :set
 'particle-process-material+set-param-min)

(defgproperty particle-process-material+anim-offset-max
 'particle-process-material :index 11 :get
 'particle-process-material+get-param-max :set
 'particle-process-material+set-param-max)

(defgproperty particle-process-material+anim-offset-curve
 'particle-process-material :index 11 :get
 'particle-process-material+get-param-texture :set
 'particle-process-material+set-param-texture)

(defgproperty particle-process-material+turbulence-enabled
 'particle-process-material :get
 'particle-process-material+get-turbulence-enabled :set
 'particle-process-material+set-turbulence-enabled)

(defgproperty particle-process-material+turbulence-noise-strength
 'particle-process-material :get
 'particle-process-material+get-turbulence-noise-strength :set
 'particle-process-material+set-turbulence-noise-strength)

(defgproperty particle-process-material+turbulence-noise-scale
 'particle-process-material :get
 'particle-process-material+get-turbulence-noise-scale :set
 'particle-process-material+set-turbulence-noise-scale)

(defgproperty particle-process-material+turbulence-noise-speed
 'particle-process-material :get
 'particle-process-material+get-turbulence-noise-speed :set
 'particle-process-material+set-turbulence-noise-speed)

(defgproperty particle-process-material+turbulence-noise-speed-random
 'particle-process-material :get
 'particle-process-material+get-turbulence-noise-speed-random :set
 'particle-process-material+set-turbulence-noise-speed-random)

(defgproperty particle-process-material+turbulence-influence
 'particle-process-material :index 13 :get 'particle-process-material+get-param
 :set 'particle-process-material+set-param)

(defgproperty particle-process-material+turbulence-influence-min
 'particle-process-material :index 13 :get
 'particle-process-material+get-param-min :set
 'particle-process-material+set-param-min)

(defgproperty particle-process-material+turbulence-influence-max
 'particle-process-material :index 13 :get
 'particle-process-material+get-param-max :set
 'particle-process-material+set-param-max)

(defgproperty particle-process-material+turbulence-initial-displacement
 'particle-process-material :index 14 :get 'particle-process-material+get-param
 :set 'particle-process-material+set-param)

(defgproperty particle-process-material+turbulence-initial-displacement-min
 'particle-process-material :index 14 :get
 'particle-process-material+get-param-min :set
 'particle-process-material+set-param-min)

(defgproperty particle-process-material+turbulence-initial-displacement-max
 'particle-process-material :index 14 :get
 'particle-process-material+get-param-max :set
 'particle-process-material+set-param-max)

(defgproperty particle-process-material+turbulence-influence-over-life
 'particle-process-material :index 12 :get
 'particle-process-material+get-param-texture :set
 'particle-process-material+set-param-texture)

(defgproperty particle-process-material+collision-mode
 'particle-process-material :get 'particle-process-material+get-collision-mode
 :set 'particle-process-material+set-collision-mode)

(defgproperty particle-process-material+collision-friction
 'particle-process-material :get
 'particle-process-material+get-collision-friction :set
 'particle-process-material+set-collision-friction)

(defgproperty particle-process-material+collision-bounce
 'particle-process-material :get
 'particle-process-material+get-collision-bounce :set
 'particle-process-material+set-collision-bounce)

(defgproperty particle-process-material+collision-use-scale
 'particle-process-material :get
 'particle-process-material+is-collision-using-scale :set
 'particle-process-material+set-collision-use-scale)

(defgproperty particle-process-material+sub-emitter-mode
 'particle-process-material :get
 'particle-process-material+get-sub-emitter-mode :set
 'particle-process-material+set-sub-emitter-mode)

(defgproperty particle-process-material+sub-emitter-frequency
 'particle-process-material :get
 'particle-process-material+get-sub-emitter-frequency :set
 'particle-process-material+set-sub-emitter-frequency)

(defgproperty particle-process-material+sub-emitter-amount-at-end
 'particle-process-material :get
 'particle-process-material+get-sub-emitter-amount-at-end :set
 'particle-process-material+set-sub-emitter-amount-at-end)

(defgproperty particle-process-material+sub-emitter-amount-at-collision
 'particle-process-material :get
 'particle-process-material+get-sub-emitter-amount-at-collision :set
 'particle-process-material+set-sub-emitter-amount-at-collision)

(defgproperty particle-process-material+sub-emitter-amount-at-start
 'particle-process-material :get
 'particle-process-material+get-sub-emitter-amount-at-start :set
 'particle-process-material+set-sub-emitter-amount-at-start)

(defgproperty particle-process-material+sub-emitter-keep-velocity
 'particle-process-material :get
 'particle-process-material+get-sub-emitter-keep-velocity :set
 'particle-process-material+set-sub-emitter-keep-velocity)