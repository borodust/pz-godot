(common-lisp:in-package :%godot)


(defgmethod
 (tile-set-scenes-collection-source+get-scene-tiles-count :class
  'tile-set-scenes-collection-source :bind "get_scene_tiles_count" :hash
  2455072627)
 int)

(defgmethod
 (tile-set-scenes-collection-source+get-scene-tile-id :class
  'tile-set-scenes-collection-source :bind "get_scene_tile_id" :hash
  3744713108)
 int (index int))

(defgmethod
 (tile-set-scenes-collection-source+has-scene-tile-id :class
  'tile-set-scenes-collection-source :bind "has_scene_tile_id" :hash
  3067735520)
 bool (id int))

(defgmethod
 (tile-set-scenes-collection-source+create-scene-tile :class
  'tile-set-scenes-collection-source :bind "create_scene_tile" :hash
  1117465415)
 int (packed-scene packed-scene) (id-override int))

(defgmethod
 (tile-set-scenes-collection-source+set-scene-tile-id :class
  'tile-set-scenes-collection-source :bind "set_scene_tile_id" :hash
  3937882851)
 :void (id int) (new-id int))

(defgmethod
 (tile-set-scenes-collection-source+set-scene-tile-scene :class
  'tile-set-scenes-collection-source :bind "set_scene_tile_scene" :hash
  3435852839)
 :void (id int) (packed-scene packed-scene))

(defgmethod
 (tile-set-scenes-collection-source+get-scene-tile-scene :class
  'tile-set-scenes-collection-source :bind "get_scene_tile_scene" :hash
  511017218)
 packed-scene (id int))

(defgmethod
 (tile-set-scenes-collection-source+set-scene-tile-display-placeholder :class
  'tile-set-scenes-collection-source :bind "set_scene_tile_display_placeholder"
  :hash 300928843)
 :void (id int) (display-placeholder bool))

(defgmethod
 (tile-set-scenes-collection-source+get-scene-tile-display-placeholder :class
  'tile-set-scenes-collection-source :bind "get_scene_tile_display_placeholder"
  :hash 1116898809)
 bool (id int))

(defgmethod
 (tile-set-scenes-collection-source+remove-scene-tile :class
  'tile-set-scenes-collection-source :bind "remove_scene_tile" :hash
  1286410249)
 :void (id int))

(defgmethod
 (tile-set-scenes-collection-source+get-next-scene-tile-id :class
  'tile-set-scenes-collection-source :bind "get_next_scene_tile_id" :hash
  3905245786)
 int)