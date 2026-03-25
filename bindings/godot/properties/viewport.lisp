(common-lisp:in-package :%godot)


(defgproperty viewport+disable-3d 'viewport :get 'viewport+is-3d-disabled :set
 'viewport+set-disable-3d)

(defgproperty viewport+use-xr 'viewport :get 'viewport+is-using-xr :set
 'viewport+set-use-xr)

(defgproperty viewport+own-world-3d 'viewport :get
 'viewport+is-using-own-world-3d :set 'viewport+set-use-own-world-3d)

(defgproperty viewport+world-3d 'viewport :get 'viewport+get-world-3d :set
 'viewport+set-world-3d)

(defgproperty viewport+world-2d 'viewport :get 'viewport+get-world-2d :set
 'viewport+set-world-2d)

(defgproperty viewport+transparent-bg 'viewport :get
 'viewport+has-transparent-background :set 'viewport+set-transparent-background)

(defgproperty viewport+handle-input-locally 'viewport :get
 'viewport+is-handling-input-locally :set 'viewport+set-handle-input-locally)

(defgproperty viewport+snap-2d-transforms-to-pixel 'viewport :get
 'viewport+is-snap-2d-transforms-to-pixel-enabled :set
 'viewport+set-snap-2d-transforms-to-pixel)

(defgproperty viewport+snap-2d-vertices-to-pixel 'viewport :get
 'viewport+is-snap-2d-vertices-to-pixel-enabled :set
 'viewport+set-snap-2d-vertices-to-pixel)

(defgproperty viewport+msaa-2d 'viewport :get 'viewport+get-msaa-2d :set
 'viewport+set-msaa-2d)

(defgproperty viewport+msaa-3d 'viewport :get 'viewport+get-msaa-3d :set
 'viewport+set-msaa-3d)

(defgproperty viewport+screen-space-aa 'viewport :get
 'viewport+get-screen-space-aa :set 'viewport+set-screen-space-aa)

(defgproperty viewport+use-taa 'viewport :get 'viewport+is-using-taa :set
 'viewport+set-use-taa)

(defgproperty viewport+use-debanding 'viewport :get
 'viewport+is-using-debanding :set 'viewport+set-use-debanding)

(defgproperty viewport+use-occlusion-culling 'viewport :get
 'viewport+is-using-occlusion-culling :set 'viewport+set-use-occlusion-culling)

(defgproperty viewport+mesh-lod-threshold 'viewport :get
 'viewport+get-mesh-lod-threshold :set 'viewport+set-mesh-lod-threshold)

(defgproperty viewport+debug-draw 'viewport :get 'viewport+get-debug-draw :set
 'viewport+set-debug-draw)

(defgproperty viewport+use-hdr-2d 'viewport :get 'viewport+is-using-hdr-2d :set
 'viewport+set-use-hdr-2d)

(defgproperty viewport+scaling-3d-mode 'viewport :get
 'viewport+get-scaling-3d-mode :set 'viewport+set-scaling-3d-mode)

(defgproperty viewport+scaling-3d-scale 'viewport :get
 'viewport+get-scaling-3d-scale :set 'viewport+set-scaling-3d-scale)

(defgproperty viewport+texture-mipmap-bias 'viewport :get
 'viewport+get-texture-mipmap-bias :set 'viewport+set-texture-mipmap-bias)

(defgproperty viewport+anisotropic-filtering-level 'viewport :get
 'viewport+get-anisotropic-filtering-level :set
 'viewport+set-anisotropic-filtering-level)

(defgproperty viewport+fsr-sharpness 'viewport :get 'viewport+get-fsr-sharpness
 :set 'viewport+set-fsr-sharpness)

(defgproperty viewport+vrs-mode 'viewport :get 'viewport+get-vrs-mode :set
 'viewport+set-vrs-mode)

(defgproperty viewport+vrs-update-mode 'viewport :get
 'viewport+get-vrs-update-mode :set 'viewport+set-vrs-update-mode)

(defgproperty viewport+vrs-texture 'viewport :get 'viewport+get-vrs-texture
 :set 'viewport+set-vrs-texture)

(defgproperty viewport+canvas-item-default-texture-filter 'viewport :get
 'viewport+get-default-canvas-item-texture-filter :set
 'viewport+set-default-canvas-item-texture-filter)

(defgproperty viewport+canvas-item-default-texture-repeat 'viewport :get
 'viewport+get-default-canvas-item-texture-repeat :set
 'viewport+set-default-canvas-item-texture-repeat)

(defgproperty viewport+audio-listener-enable-2d 'viewport :get
 'viewport+is-audio-listener-2d :set 'viewport+set-as-audio-listener-2d)

(defgproperty viewport+audio-listener-enable-3d 'viewport :get
 'viewport+is-audio-listener-3d :set 'viewport+set-as-audio-listener-3d)

(defgproperty viewport+physics-object-picking 'viewport :get
 'viewport+get-physics-object-picking :set 'viewport+set-physics-object-picking)

(defgproperty viewport+physics-object-picking-sort 'viewport :get
 'viewport+get-physics-object-picking-sort :set
 'viewport+set-physics-object-picking-sort)

(defgproperty viewport+physics-object-picking-first-only 'viewport :get
 'viewport+get-physics-object-picking-first-only :set
 'viewport+set-physics-object-picking-first-only)

(defgproperty viewport+gui-disable-input 'viewport :get
 'viewport+is-input-disabled :set 'viewport+set-disable-input)

(defgproperty viewport+gui-snap-controls-to-pixels 'viewport :get
 'viewport+is-snap-controls-to-pixels-enabled :set
 'viewport+set-snap-controls-to-pixels)

(defgproperty viewport+gui-embed-subwindows 'viewport :get
 'viewport+is-embedding-subwindows :set 'viewport+set-embedding-subwindows)

(defgproperty viewport+gui-drag-threshold 'viewport :get
 'viewport+get-drag-threshold :set 'viewport+set-drag-threshold)

(defgproperty viewport+sdf-oversize 'viewport :get 'viewport+get-sdf-oversize
 :set 'viewport+set-sdf-oversize)

(defgproperty viewport+sdf-scale 'viewport :get 'viewport+get-sdf-scale :set
 'viewport+set-sdf-scale)

(defgproperty viewport+positional-shadow-atlas-size 'viewport :get
 'viewport+get-positional-shadow-atlas-size :set
 'viewport+set-positional-shadow-atlas-size)

(defgproperty viewport+positional-shadow-atlas-16-bits 'viewport :get
 'viewport+get-positional-shadow-atlas-16-bits :set
 'viewport+set-positional-shadow-atlas-16-bits)

(defgproperty viewport+positional-shadow-atlas-quad-0 'viewport :index 0 :get
 'viewport+get-positional-shadow-atlas-quadrant-subdiv :set
 'viewport+set-positional-shadow-atlas-quadrant-subdiv)

(defgproperty viewport+positional-shadow-atlas-quad-1 'viewport :index 1 :get
 'viewport+get-positional-shadow-atlas-quadrant-subdiv :set
 'viewport+set-positional-shadow-atlas-quadrant-subdiv)

(defgproperty viewport+positional-shadow-atlas-quad-2 'viewport :index 2 :get
 'viewport+get-positional-shadow-atlas-quadrant-subdiv :set
 'viewport+set-positional-shadow-atlas-quadrant-subdiv)

(defgproperty viewport+positional-shadow-atlas-quad-3 'viewport :index 3 :get
 'viewport+get-positional-shadow-atlas-quadrant-subdiv :set
 'viewport+set-positional-shadow-atlas-quadrant-subdiv)

(defgproperty viewport+canvas-transform 'viewport :get
 'viewport+get-canvas-transform :set 'viewport+set-canvas-transform)

(defgproperty viewport+global-canvas-transform 'viewport :get
 'viewport+get-global-canvas-transform :set
 'viewport+set-global-canvas-transform)

(defgproperty viewport+canvas-cull-mask 'viewport :get
 'viewport+get-canvas-cull-mask :set 'viewport+set-canvas-cull-mask)

(defgproperty viewport+oversampling 'viewport :get
 'viewport+is-using-oversampling :set 'viewport+set-use-oversampling)

(defgproperty viewport+oversampling-override 'viewport :get
 'viewport+get-oversampling-override :set 'viewport+set-oversampling-override)