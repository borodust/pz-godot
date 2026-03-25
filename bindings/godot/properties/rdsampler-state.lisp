(common-lisp:in-package :%godot)


(defgproperty rdsampler-state+mag-filter 'rdsampler-state :get
 'rdsampler-state+get-mag-filter :set 'rdsampler-state+set-mag-filter)

(defgproperty rdsampler-state+min-filter 'rdsampler-state :get
 'rdsampler-state+get-min-filter :set 'rdsampler-state+set-min-filter)

(defgproperty rdsampler-state+mip-filter 'rdsampler-state :get
 'rdsampler-state+get-mip-filter :set 'rdsampler-state+set-mip-filter)

(defgproperty rdsampler-state+repeat-u 'rdsampler-state :get
 'rdsampler-state+get-repeat-u :set 'rdsampler-state+set-repeat-u)

(defgproperty rdsampler-state+repeat-v 'rdsampler-state :get
 'rdsampler-state+get-repeat-v :set 'rdsampler-state+set-repeat-v)

(defgproperty rdsampler-state+repeat-w 'rdsampler-state :get
 'rdsampler-state+get-repeat-w :set 'rdsampler-state+set-repeat-w)

(defgproperty rdsampler-state+lod-bias 'rdsampler-state :get
 'rdsampler-state+get-lod-bias :set 'rdsampler-state+set-lod-bias)

(defgproperty rdsampler-state+use-anisotropy 'rdsampler-state :get
 'rdsampler-state+get-use-anisotropy :set 'rdsampler-state+set-use-anisotropy)

(defgproperty rdsampler-state+anisotropy-max 'rdsampler-state :get
 'rdsampler-state+get-anisotropy-max :set 'rdsampler-state+set-anisotropy-max)

(defgproperty rdsampler-state+enable-compare 'rdsampler-state :get
 'rdsampler-state+get-enable-compare :set 'rdsampler-state+set-enable-compare)

(defgproperty rdsampler-state+compare-op 'rdsampler-state :get
 'rdsampler-state+get-compare-op :set 'rdsampler-state+set-compare-op)

(defgproperty rdsampler-state+min-lod 'rdsampler-state :get
 'rdsampler-state+get-min-lod :set 'rdsampler-state+set-min-lod)

(defgproperty rdsampler-state+max-lod 'rdsampler-state :get
 'rdsampler-state+get-max-lod :set 'rdsampler-state+set-max-lod)

(defgproperty rdsampler-state+border-color 'rdsampler-state :get
 'rdsampler-state+get-border-color :set 'rdsampler-state+set-border-color)

(defgproperty rdsampler-state+unnormalized-uvw 'rdsampler-state :get
 'rdsampler-state+get-unnormalized-uvw :set
 'rdsampler-state+set-unnormalized-uvw)