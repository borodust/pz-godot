(common-lisp:in-package :%godot)


(defgmethod
 (rdsampler-state+set-mag-filter :class 'rdsampler-state :bind "set_mag_filter"
  :hash 1493420382)
 :void (p-member rendering-device+sampler-filter))

(defgmethod
 (rdsampler-state+get-mag-filter :class 'rdsampler-state :bind "get_mag_filter"
  :hash 2209202801)
 rendering-device+sampler-filter)

(defgmethod
 (rdsampler-state+set-min-filter :class 'rdsampler-state :bind "set_min_filter"
  :hash 1493420382)
 :void (p-member rendering-device+sampler-filter))

(defgmethod
 (rdsampler-state+get-min-filter :class 'rdsampler-state :bind "get_min_filter"
  :hash 2209202801)
 rendering-device+sampler-filter)

(defgmethod
 (rdsampler-state+set-mip-filter :class 'rdsampler-state :bind "set_mip_filter"
  :hash 1493420382)
 :void (p-member rendering-device+sampler-filter))

(defgmethod
 (rdsampler-state+get-mip-filter :class 'rdsampler-state :bind "get_mip_filter"
  :hash 2209202801)
 rendering-device+sampler-filter)

(defgmethod
 (rdsampler-state+set-repeat-u :class 'rdsampler-state :bind "set_repeat_u"
  :hash 246127626)
 :void (p-member rendering-device+sampler-repeat-mode))

(defgmethod
 (rdsampler-state+get-repeat-u :class 'rdsampler-state :bind "get_repeat_u"
  :hash 3227895872)
 rendering-device+sampler-repeat-mode)

(defgmethod
 (rdsampler-state+set-repeat-v :class 'rdsampler-state :bind "set_repeat_v"
  :hash 246127626)
 :void (p-member rendering-device+sampler-repeat-mode))

(defgmethod
 (rdsampler-state+get-repeat-v :class 'rdsampler-state :bind "get_repeat_v"
  :hash 3227895872)
 rendering-device+sampler-repeat-mode)

(defgmethod
 (rdsampler-state+set-repeat-w :class 'rdsampler-state :bind "set_repeat_w"
  :hash 246127626)
 :void (p-member rendering-device+sampler-repeat-mode))

(defgmethod
 (rdsampler-state+get-repeat-w :class 'rdsampler-state :bind "get_repeat_w"
  :hash 3227895872)
 rendering-device+sampler-repeat-mode)

(defgmethod
 (rdsampler-state+set-lod-bias :class 'rdsampler-state :bind "set_lod_bias"
  :hash 373806689)
 :void (p-member float))

(defgmethod
 (rdsampler-state+get-lod-bias :class 'rdsampler-state :bind "get_lod_bias"
  :hash 1740695150)
 float)

(defgmethod
 (rdsampler-state+set-use-anisotropy :class 'rdsampler-state :bind
  "set_use_anisotropy" :hash 2586408642)
 :void (p-member bool))

(defgmethod
 (rdsampler-state+get-use-anisotropy :class 'rdsampler-state :bind
  "get_use_anisotropy" :hash 36873697)
 bool)

(defgmethod
 (rdsampler-state+set-anisotropy-max :class 'rdsampler-state :bind
  "set_anisotropy_max" :hash 373806689)
 :void (p-member float))

(defgmethod
 (rdsampler-state+get-anisotropy-max :class 'rdsampler-state :bind
  "get_anisotropy_max" :hash 1740695150)
 float)

(defgmethod
 (rdsampler-state+set-enable-compare :class 'rdsampler-state :bind
  "set_enable_compare" :hash 2586408642)
 :void (p-member bool))

(defgmethod
 (rdsampler-state+get-enable-compare :class 'rdsampler-state :bind
  "get_enable_compare" :hash 36873697)
 bool)

(defgmethod
 (rdsampler-state+set-compare-op :class 'rdsampler-state :bind "set_compare_op"
  :hash 2573711505)
 :void (p-member rendering-device+compare-operator))

(defgmethod
 (rdsampler-state+get-compare-op :class 'rdsampler-state :bind "get_compare_op"
  :hash 269730778)
 rendering-device+compare-operator)

(defgmethod
 (rdsampler-state+set-min-lod :class 'rdsampler-state :bind "set_min_lod" :hash
  373806689)
 :void (p-member float))

(defgmethod
 (rdsampler-state+get-min-lod :class 'rdsampler-state :bind "get_min_lod" :hash
  1740695150)
 float)

(defgmethod
 (rdsampler-state+set-max-lod :class 'rdsampler-state :bind "set_max_lod" :hash
  373806689)
 :void (p-member float))

(defgmethod
 (rdsampler-state+get-max-lod :class 'rdsampler-state :bind "get_max_lod" :hash
  1740695150)
 float)

(defgmethod
 (rdsampler-state+set-border-color :class 'rdsampler-state :bind
  "set_border_color" :hash 1115869595)
 :void (p-member rendering-device+sampler-border-color))

(defgmethod
 (rdsampler-state+get-border-color :class 'rdsampler-state :bind
  "get_border_color" :hash 3514246478)
 rendering-device+sampler-border-color)

(defgmethod
 (rdsampler-state+set-unnormalized-uvw :class 'rdsampler-state :bind
  "set_unnormalized_uvw" :hash 2586408642)
 :void (p-member bool))

(defgmethod
 (rdsampler-state+get-unnormalized-uvw :class 'rdsampler-state :bind
  "get_unnormalized_uvw" :hash 36873697)
 bool)