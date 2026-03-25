(common-lisp:in-package :%godot)


(defgmethod
 (navigation-mesh-generator+bake :class 'navigation-mesh-generator :bind "bake"
  :hash 1401173477)
 :void (navigation-mesh navigation-mesh) (root-node node))

(defgmethod
 (navigation-mesh-generator+clear :class 'navigation-mesh-generator :bind
  "clear" :hash 2923361153)
 :void (navigation-mesh navigation-mesh))

(defgmethod
 (navigation-mesh-generator+parse-source-geometry-data :class
  'navigation-mesh-generator :bind "parse_source_geometry_data" :hash
  3172802542)
 :void (navigation-mesh navigation-mesh)
 (source-geometry-data navigation-mesh-source-geometry-data-3d)
 (root-node node) (callback callable))

(defgmethod
 (navigation-mesh-generator+bake-from-source-geometry-data :class
  'navigation-mesh-generator :bind "bake_from_source_geometry_data" :hash
  1286748856)
 :void (navigation-mesh navigation-mesh)
 (source-geometry-data navigation-mesh-source-geometry-data-3d)
 (callback callable))