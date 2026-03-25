(common-lisp:in-package :%godot)


(defgmethod
 (tile-set-source+get-tiles-count :class 'tile-set-source :bind
  "get_tiles_count" :hash 3905245786)
 int)

(defgmethod
 (tile-set-source+get-tile-id :class 'tile-set-source :bind "get_tile_id" :hash
  880721226)
 vector-2i (index int))

(defgmethod
 (tile-set-source+has-tile :class 'tile-set-source :bind "has_tile" :hash
  3900751641)
 bool (atlas-coords vector-2i))

(defgmethod
 (tile-set-source+get-alternative-tiles-count :class 'tile-set-source :bind
  "get_alternative_tiles_count" :hash 2485466453)
 int (atlas-coords vector-2i))

(defgmethod
 (tile-set-source+get-alternative-tile-id :class 'tile-set-source :bind
  "get_alternative_tile_id" :hash 89881719)
 int (atlas-coords vector-2i) (index int))

(defgmethod
 (tile-set-source+has-alternative-tile :class 'tile-set-source :bind
  "has_alternative_tile" :hash 1073731340)
 bool (atlas-coords vector-2i) (alternative-tile int))