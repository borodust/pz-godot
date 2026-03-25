(common-lisp:in-package :%godot)


(defgproperty importer-mesh-instance-3d+mesh 'importer-mesh-instance-3d :get
 'importer-mesh-instance-3d+get-mesh :set 'importer-mesh-instance-3d+set-mesh)

(defgproperty importer-mesh-instance-3d+skin 'importer-mesh-instance-3d :get
 'importer-mesh-instance-3d+get-skin :set 'importer-mesh-instance-3d+set-skin)

(defgproperty importer-mesh-instance-3d+skeleton-path
 'importer-mesh-instance-3d :get 'importer-mesh-instance-3d+get-skeleton-path
 :set 'importer-mesh-instance-3d+set-skeleton-path)

(defgproperty importer-mesh-instance-3d+layer-mask 'importer-mesh-instance-3d
 :get 'importer-mesh-instance-3d+get-layer-mask :set
 'importer-mesh-instance-3d+set-layer-mask)

(defgproperty importer-mesh-instance-3d+cast-shadow 'importer-mesh-instance-3d
 :get 'importer-mesh-instance-3d+get-cast-shadows-setting :set
 'importer-mesh-instance-3d+set-cast-shadows-setting)

(defgproperty importer-mesh-instance-3d+visibility-range-begin
 'importer-mesh-instance-3d :get
 'importer-mesh-instance-3d+get-visibility-range-begin :set
 'importer-mesh-instance-3d+set-visibility-range-begin)

(defgproperty importer-mesh-instance-3d+visibility-range-begin-margin
 'importer-mesh-instance-3d :get
 'importer-mesh-instance-3d+get-visibility-range-begin-margin :set
 'importer-mesh-instance-3d+set-visibility-range-begin-margin)

(defgproperty importer-mesh-instance-3d+visibility-range-end
 'importer-mesh-instance-3d :get
 'importer-mesh-instance-3d+get-visibility-range-end :set
 'importer-mesh-instance-3d+set-visibility-range-end)

(defgproperty importer-mesh-instance-3d+visibility-range-end-margin
 'importer-mesh-instance-3d :get
 'importer-mesh-instance-3d+get-visibility-range-end-margin :set
 'importer-mesh-instance-3d+set-visibility-range-end-margin)

(defgproperty importer-mesh-instance-3d+visibility-range-fade-mode
 'importer-mesh-instance-3d :get
 'importer-mesh-instance-3d+get-visibility-range-fade-mode :set
 'importer-mesh-instance-3d+set-visibility-range-fade-mode)