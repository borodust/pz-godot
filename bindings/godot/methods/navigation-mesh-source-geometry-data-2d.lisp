(common-lisp:in-package :%godot)


(defgmethod
 (navigation-mesh-source-geometry-data-2d+clear :class
  'navigation-mesh-source-geometry-data-2d :bind "clear" :hash 3218959716)
 :void)

(defgmethod
 (navigation-mesh-source-geometry-data-2d+has-data :class
  'navigation-mesh-source-geometry-data-2d :bind "has_data" :hash 2240911060)
 bool)

(defgmethod
 (navigation-mesh-source-geometry-data-2d+set-traversable-outlines :class
  'navigation-mesh-source-geometry-data-2d :bind "set_traversable_outlines"
  :hash 381264803)
 :void (traversable-outlines array))

(defgmethod
 (navigation-mesh-source-geometry-data-2d+get-traversable-outlines :class
  'navigation-mesh-source-geometry-data-2d :bind "get_traversable_outlines"
  :hash 3995934104)
 array)

(defgmethod
 (navigation-mesh-source-geometry-data-2d+set-obstruction-outlines :class
  'navigation-mesh-source-geometry-data-2d :bind "set_obstruction_outlines"
  :hash 381264803)
 :void (obstruction-outlines array))

(defgmethod
 (navigation-mesh-source-geometry-data-2d+get-obstruction-outlines :class
  'navigation-mesh-source-geometry-data-2d :bind "get_obstruction_outlines"
  :hash 3995934104)
 array)

(defgmethod
 (navigation-mesh-source-geometry-data-2d+append-traversable-outlines :class
  'navigation-mesh-source-geometry-data-2d :bind "append_traversable_outlines"
  :hash 381264803)
 :void (traversable-outlines array))

(defgmethod
 (navigation-mesh-source-geometry-data-2d+append-obstruction-outlines :class
  'navigation-mesh-source-geometry-data-2d :bind "append_obstruction_outlines"
  :hash 381264803)
 :void (obstruction-outlines array))

(defgmethod
 (navigation-mesh-source-geometry-data-2d+add-traversable-outline :class
  'navigation-mesh-source-geometry-data-2d :bind "add_traversable_outline"
  :hash 1509147220)
 :void (shape-outline packed-vector-2array))

(defgmethod
 (navigation-mesh-source-geometry-data-2d+add-obstruction-outline :class
  'navigation-mesh-source-geometry-data-2d :bind "add_obstruction_outline"
  :hash 1509147220)
 :void (shape-outline packed-vector-2array))

(defgmethod
 (navigation-mesh-source-geometry-data-2d+merge :class
  'navigation-mesh-source-geometry-data-2d :bind "merge" :hash 742424872)
 :void (other-geometry navigation-mesh-source-geometry-data-2d))

(defgmethod
 (navigation-mesh-source-geometry-data-2d+add-projected-obstruction :class
  'navigation-mesh-source-geometry-data-2d :bind "add_projected_obstruction"
  :hash 3882407395)
 :void (vertices packed-vector-2array) (carve bool))

(defgmethod
 (navigation-mesh-source-geometry-data-2d+clear-projected-obstructions :class
  'navigation-mesh-source-geometry-data-2d :bind "clear_projected_obstructions"
  :hash 3218959716)
 :void)

(defgmethod
 (navigation-mesh-source-geometry-data-2d+set-projected-obstructions :class
  'navigation-mesh-source-geometry-data-2d :bind "set_projected_obstructions"
  :hash 381264803)
 :void (projected-obstructions array))

(defgmethod
 (navigation-mesh-source-geometry-data-2d+get-projected-obstructions :class
  'navigation-mesh-source-geometry-data-2d :bind "get_projected_obstructions"
  :hash 3995934104)
 array)

(defgmethod
 (navigation-mesh-source-geometry-data-2d+get-bounds :class
  'navigation-mesh-source-geometry-data-2d :bind "get_bounds" :hash 3248174)
 rect-2)