(common-lisp:in-package :%godot)


(defgmethod
 (noise+get-noise-1d :class 'noise :bind "get_noise_1d" :hash 3919130443) float
 (x float))

(defgmethod
 (noise+get-noise-2d :class 'noise :bind "get_noise_2d" :hash 2753205203) float
 (x float) (y float))

(defgmethod
 (noise+get-noise-2dv :class 'noise :bind "get_noise_2dv" :hash 2276447920)
 float (v vector-2))

(defgmethod
 (noise+get-noise-3d :class 'noise :bind "get_noise_3d" :hash 973811851) float
 (x float) (y float) (z float))

(defgmethod
 (noise+get-noise-3dv :class 'noise :bind "get_noise_3dv" :hash 1109078154)
 float (v vector-3))

(defgmethod (noise+get-image :class 'noise :bind "get_image" :hash 3180683109)
 image (width int) (height int) (invert bool) (in-3d-space bool)
 (normalize bool))

(defgmethod
 (noise+get-seamless-image :class 'noise :bind "get_seamless_image" :hash
  2770743602)
 image (width int) (height int) (invert bool) (in-3d-space bool) (skirt float)
 (normalize bool))

(defgmethod
 (noise+get-image-3d :class 'noise :bind "get_image_3d" :hash 3977814329) array
 (width int) (height int) (depth int) (invert bool) (normalize bool))

(defgmethod
 (noise+get-seamless-image-3d :class 'noise :bind "get_seamless_image_3d" :hash
  451006340)
 array (width int) (height int) (depth int) (invert bool) (skirt float)
 (normalize bool))