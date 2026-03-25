(common-lisp:in-package :%godot)


(defgproperty geometry-instance-3d+material-override 'geometry-instance-3d :get
 'geometry-instance-3d+get-material-override :set
 'geometry-instance-3d+set-material-override)

(defgproperty geometry-instance-3d+material-overlay 'geometry-instance-3d :get
 'geometry-instance-3d+get-material-overlay :set
 'geometry-instance-3d+set-material-overlay)

(defgproperty geometry-instance-3d+transparency 'geometry-instance-3d :get
 'geometry-instance-3d+get-transparency :set
 'geometry-instance-3d+set-transparency)

(defgproperty geometry-instance-3d+cast-shadow 'geometry-instance-3d :get
 'geometry-instance-3d+get-cast-shadows-setting :set
 'geometry-instance-3d+set-cast-shadows-setting)

(defgproperty geometry-instance-3d+extra-cull-margin 'geometry-instance-3d :get
 'geometry-instance-3d+get-extra-cull-margin :set
 'geometry-instance-3d+set-extra-cull-margin)

(defgproperty geometry-instance-3d+custom-aabb 'geometry-instance-3d :get
 'geometry-instance-3d+get-custom-aabb :set
 'geometry-instance-3d+set-custom-aabb)

(defgproperty geometry-instance-3d+lod-bias 'geometry-instance-3d :get
 'geometry-instance-3d+get-lod-bias :set 'geometry-instance-3d+set-lod-bias)

(defgproperty geometry-instance-3d+ignore-occlusion-culling
 'geometry-instance-3d :get 'geometry-instance-3d+is-ignoring-occlusion-culling
 :set 'geometry-instance-3d+set-ignore-occlusion-culling)

(defgproperty geometry-instance-3d+gi-mode 'geometry-instance-3d :get
 'geometry-instance-3d+get-gi-mode :set 'geometry-instance-3d+set-gi-mode)

(defgproperty geometry-instance-3d+gi-lightmap-texel-scale
 'geometry-instance-3d :get 'geometry-instance-3d+get-lightmap-texel-scale :set
 'geometry-instance-3d+set-lightmap-texel-scale)

(defgproperty geometry-instance-3d+gi-lightmap-scale 'geometry-instance-3d :get
 'geometry-instance-3d+get-lightmap-scale :set
 'geometry-instance-3d+set-lightmap-scale)

(defgproperty geometry-instance-3d+visibility-range-begin 'geometry-instance-3d
 :get 'geometry-instance-3d+get-visibility-range-begin :set
 'geometry-instance-3d+set-visibility-range-begin)

(defgproperty geometry-instance-3d+visibility-range-begin-margin
 'geometry-instance-3d :get
 'geometry-instance-3d+get-visibility-range-begin-margin :set
 'geometry-instance-3d+set-visibility-range-begin-margin)

(defgproperty geometry-instance-3d+visibility-range-end 'geometry-instance-3d
 :get 'geometry-instance-3d+get-visibility-range-end :set
 'geometry-instance-3d+set-visibility-range-end)

(defgproperty geometry-instance-3d+visibility-range-end-margin
 'geometry-instance-3d :get
 'geometry-instance-3d+get-visibility-range-end-margin :set
 'geometry-instance-3d+set-visibility-range-end-margin)

(defgproperty geometry-instance-3d+visibility-range-fade-mode
 'geometry-instance-3d :get
 'geometry-instance-3d+get-visibility-range-fade-mode :set
 'geometry-instance-3d+set-visibility-range-fade-mode)