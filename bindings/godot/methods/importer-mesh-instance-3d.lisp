(common-lisp:in-package :%godot)


(defgmethod
 (importer-mesh-instance-3d+set-mesh :class 'importer-mesh-instance-3d :bind
  "set_mesh" :hash 2255166972)
 :void (mesh importer-mesh))

(defgmethod
 (importer-mesh-instance-3d+get-mesh :class 'importer-mesh-instance-3d :bind
  "get_mesh" :hash 3161779525)
 importer-mesh)

(defgmethod
 (importer-mesh-instance-3d+set-skin :class 'importer-mesh-instance-3d :bind
  "set_skin" :hash 3971435618)
 :void (skin skin))

(defgmethod
 (importer-mesh-instance-3d+get-skin :class 'importer-mesh-instance-3d :bind
  "get_skin" :hash 2074563878)
 skin)

(defgmethod
 (importer-mesh-instance-3d+set-skeleton-path :class 'importer-mesh-instance-3d
  :bind "set_skeleton_path" :hash 1348162250)
 :void (skeleton-path node-path))

(defgmethod
 (importer-mesh-instance-3d+get-skeleton-path :class 'importer-mesh-instance-3d
  :bind "get_skeleton_path" :hash 4075236667)
 node-path)

(defgmethod
 (importer-mesh-instance-3d+set-layer-mask :class 'importer-mesh-instance-3d
  :bind "set_layer_mask" :hash 1286410249)
 :void (layer-mask int))

(defgmethod
 (importer-mesh-instance-3d+get-layer-mask :class 'importer-mesh-instance-3d
  :bind "get_layer_mask" :hash 3905245786)
 int)

(defgmethod
 (importer-mesh-instance-3d+set-cast-shadows-setting :class
  'importer-mesh-instance-3d :bind "set_cast_shadows_setting" :hash 856677339)
 :void (shadow-casting-setting geometry-instance-3d+shadow-casting-setting))

(defgmethod
 (importer-mesh-instance-3d+get-cast-shadows-setting :class
  'importer-mesh-instance-3d :bind "get_cast_shadows_setting" :hash 3383019359)
 geometry-instance-3d+shadow-casting-setting)

(defgmethod
 (importer-mesh-instance-3d+set-visibility-range-end-margin :class
  'importer-mesh-instance-3d :bind "set_visibility_range_end_margin" :hash
  373806689)
 :void (distance float))

(defgmethod
 (importer-mesh-instance-3d+get-visibility-range-end-margin :class
  'importer-mesh-instance-3d :bind "get_visibility_range_end_margin" :hash
  1740695150)
 float)

(defgmethod
 (importer-mesh-instance-3d+set-visibility-range-end :class
  'importer-mesh-instance-3d :bind "set_visibility_range_end" :hash 373806689)
 :void (distance float))

(defgmethod
 (importer-mesh-instance-3d+get-visibility-range-end :class
  'importer-mesh-instance-3d :bind "get_visibility_range_end" :hash 1740695150)
 float)

(defgmethod
 (importer-mesh-instance-3d+set-visibility-range-begin-margin :class
  'importer-mesh-instance-3d :bind "set_visibility_range_begin_margin" :hash
  373806689)
 :void (distance float))

(defgmethod
 (importer-mesh-instance-3d+get-visibility-range-begin-margin :class
  'importer-mesh-instance-3d :bind "get_visibility_range_begin_margin" :hash
  1740695150)
 float)

(defgmethod
 (importer-mesh-instance-3d+set-visibility-range-begin :class
  'importer-mesh-instance-3d :bind "set_visibility_range_begin" :hash
  373806689)
 :void (distance float))

(defgmethod
 (importer-mesh-instance-3d+get-visibility-range-begin :class
  'importer-mesh-instance-3d :bind "get_visibility_range_begin" :hash
  1740695150)
 float)

(defgmethod
 (importer-mesh-instance-3d+set-visibility-range-fade-mode :class
  'importer-mesh-instance-3d :bind "set_visibility_range_fade_mode" :hash
  1440117808)
 :void (mode geometry-instance-3d+visibility-range-fade-mode))

(defgmethod
 (importer-mesh-instance-3d+get-visibility-range-fade-mode :class
  'importer-mesh-instance-3d :bind "get_visibility_range_fade_mode" :hash
  2067221882)
 geometry-instance-3d+visibility-range-fade-mode)