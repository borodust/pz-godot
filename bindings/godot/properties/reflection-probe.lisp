(common-lisp:in-package :%godot)


(defgproperty reflection-probe+update-mode 'reflection-probe :get
 'reflection-probe+get-update-mode :set 'reflection-probe+set-update-mode)

(defgproperty reflection-probe+intensity 'reflection-probe :get
 'reflection-probe+get-intensity :set 'reflection-probe+set-intensity)

(defgproperty reflection-probe+blend-distance 'reflection-probe :get
 'reflection-probe+get-blend-distance :set 'reflection-probe+set-blend-distance)

(defgproperty reflection-probe+max-distance 'reflection-probe :get
 'reflection-probe+get-max-distance :set 'reflection-probe+set-max-distance)

(defgproperty reflection-probe+size 'reflection-probe :get
 'reflection-probe+get-size :set 'reflection-probe+set-size)

(defgproperty reflection-probe+origin-offset 'reflection-probe :get
 'reflection-probe+get-origin-offset :set 'reflection-probe+set-origin-offset)

(defgproperty reflection-probe+box-projection 'reflection-probe :get
 'reflection-probe+is-box-projection-enabled :set
 'reflection-probe+set-enable-box-projection)

(defgproperty reflection-probe+interior 'reflection-probe :get
 'reflection-probe+is-set-as-interior :set 'reflection-probe+set-as-interior)

(defgproperty reflection-probe+enable-shadows 'reflection-probe :get
 'reflection-probe+are-shadows-enabled :set
 'reflection-probe+set-enable-shadows)

(defgproperty reflection-probe+cull-mask 'reflection-probe :get
 'reflection-probe+get-cull-mask :set 'reflection-probe+set-cull-mask)

(defgproperty reflection-probe+reflection-mask 'reflection-probe :get
 'reflection-probe+get-reflection-mask :set
 'reflection-probe+set-reflection-mask)

(defgproperty reflection-probe+mesh-lod-threshold 'reflection-probe :get
 'reflection-probe+get-mesh-lod-threshold :set
 'reflection-probe+set-mesh-lod-threshold)

(defgproperty reflection-probe+ambient-mode 'reflection-probe :get
 'reflection-probe+get-ambient-mode :set 'reflection-probe+set-ambient-mode)

(defgproperty reflection-probe+ambient-color 'reflection-probe :get
 'reflection-probe+get-ambient-color :set 'reflection-probe+set-ambient-color)

(defgproperty reflection-probe+ambient-color-energy 'reflection-probe :get
 'reflection-probe+get-ambient-color-energy :set
 'reflection-probe+set-ambient-color-energy)