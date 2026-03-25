(common-lisp:in-package :%godot)


(defgproperty decal+size 'decal :get 'decal+get-size :set 'decal+set-size)

(defgproperty decal+texture-albedo 'decal :index 0 :get 'decal+get-texture :set
 'decal+set-texture)

(defgproperty decal+texture-normal 'decal :index 1 :get 'decal+get-texture :set
 'decal+set-texture)

(defgproperty decal+texture-orm 'decal :index 2 :get 'decal+get-texture :set
 'decal+set-texture)

(defgproperty decal+texture-emission 'decal :index 3 :get 'decal+get-texture
 :set 'decal+set-texture)

(defgproperty decal+emission-energy 'decal :get 'decal+get-emission-energy :set
 'decal+set-emission-energy)

(defgproperty decal+modulate 'decal :get 'decal+get-modulate :set
 'decal+set-modulate)

(defgproperty decal+albedo-mix 'decal :get 'decal+get-albedo-mix :set
 'decal+set-albedo-mix)

(defgproperty decal+normal-fade 'decal :get 'decal+get-normal-fade :set
 'decal+set-normal-fade)

(defgproperty decal+upper-fade 'decal :get 'decal+get-upper-fade :set
 'decal+set-upper-fade)

(defgproperty decal+lower-fade 'decal :get 'decal+get-lower-fade :set
 'decal+set-lower-fade)

(defgproperty decal+distance-fade-enabled 'decal :get
 'decal+is-distance-fade-enabled :set 'decal+set-enable-distance-fade)

(defgproperty decal+distance-fade-begin 'decal :get
 'decal+get-distance-fade-begin :set 'decal+set-distance-fade-begin)

(defgproperty decal+distance-fade-length 'decal :get
 'decal+get-distance-fade-length :set 'decal+set-distance-fade-length)

(defgproperty decal+cull-mask 'decal :get 'decal+get-cull-mask :set
 'decal+set-cull-mask)