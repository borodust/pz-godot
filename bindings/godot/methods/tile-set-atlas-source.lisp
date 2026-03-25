(common-lisp:in-package :%godot)


(defgmethod
 (tile-set-atlas-source+set-texture :class 'tile-set-atlas-source :bind
  "set_texture" :hash 4051416890)
 :void (texture texture-2d))

(defgmethod
 (tile-set-atlas-source+get-texture :class 'tile-set-atlas-source :bind
  "get_texture" :hash 3635182373)
 texture-2d)

(defgmethod
 (tile-set-atlas-source+set-margins :class 'tile-set-atlas-source :bind
  "set_margins" :hash 1130785943)
 :void (margins vector-2i))

(defgmethod
 (tile-set-atlas-source+get-margins :class 'tile-set-atlas-source :bind
  "get_margins" :hash 3690982128)
 vector-2i)

(defgmethod
 (tile-set-atlas-source+set-separation :class 'tile-set-atlas-source :bind
  "set_separation" :hash 1130785943)
 :void (separation vector-2i))

(defgmethod
 (tile-set-atlas-source+get-separation :class 'tile-set-atlas-source :bind
  "get_separation" :hash 3690982128)
 vector-2i)

(defgmethod
 (tile-set-atlas-source+set-texture-region-size :class 'tile-set-atlas-source
  :bind "set_texture_region_size" :hash 1130785943)
 :void (texture-region-size vector-2i))

(defgmethod
 (tile-set-atlas-source+get-texture-region-size :class 'tile-set-atlas-source
  :bind "get_texture_region_size" :hash 3690982128)
 vector-2i)

(defgmethod
 (tile-set-atlas-source+set-use-texture-padding :class 'tile-set-atlas-source
  :bind "set_use_texture_padding" :hash 2586408642)
 :void (use-texture-padding bool))

(defgmethod
 (tile-set-atlas-source+get-use-texture-padding :class 'tile-set-atlas-source
  :bind "get_use_texture_padding" :hash 36873697)
 bool)

(defgmethod
 (tile-set-atlas-source+create-tile :class 'tile-set-atlas-source :bind
  "create_tile" :hash 190528769)
 :void (atlas-coords vector-2i) (size vector-2i))

(defgmethod
 (tile-set-atlas-source+remove-tile :class 'tile-set-atlas-source :bind
  "remove_tile" :hash 1130785943)
 :void (atlas-coords vector-2i))

(defgmethod
 (tile-set-atlas-source+move-tile-in-atlas :class 'tile-set-atlas-source :bind
  "move_tile_in_atlas" :hash 3870111920)
 :void (atlas-coords vector-2i) (new-atlas-coords vector-2i)
 (new-size vector-2i))

(defgmethod
 (tile-set-atlas-source+get-tile-size-in-atlas :class 'tile-set-atlas-source
  :bind "get_tile_size_in_atlas" :hash 3050897911)
 vector-2i (atlas-coords vector-2i))

(defgmethod
 (tile-set-atlas-source+has-room-for-tile :class 'tile-set-atlas-source :bind
  "has_room_for_tile" :hash 3018597268)
 bool (atlas-coords vector-2i) (size vector-2i) (animation-columns int)
 (animation-separation vector-2i) (frames-count int) (ignored-tile vector-2i))

(defgmethod
 (tile-set-atlas-source+get-tiles-to-be-removed-on-change :class
  'tile-set-atlas-source :bind "get_tiles_to_be_removed_on_change" :hash
  1240378054)
 packed-vector-2array (texture texture-2d) (margins vector-2i)
 (separation vector-2i) (texture-region-size vector-2i))

(defgmethod
 (tile-set-atlas-source+get-tile-at-coords :class 'tile-set-atlas-source :bind
  "get_tile_at_coords" :hash 3050897911)
 vector-2i (atlas-coords vector-2i))

(defgmethod
 (tile-set-atlas-source+has-tiles-outside-texture :class 'tile-set-atlas-source
  :bind "has_tiles_outside_texture" :hash 36873697)
 bool)

(defgmethod
 (tile-set-atlas-source+clear-tiles-outside-texture :class
  'tile-set-atlas-source :bind "clear_tiles_outside_texture" :hash 3218959716)
 :void)

(defgmethod
 (tile-set-atlas-source+set-tile-animation-columns :class
  'tile-set-atlas-source :bind "set_tile_animation_columns" :hash 3200960707)
 :void (atlas-coords vector-2i) (frame-columns int))

(defgmethod
 (tile-set-atlas-source+get-tile-animation-columns :class
  'tile-set-atlas-source :bind "get_tile_animation_columns" :hash 2485466453)
 int (atlas-coords vector-2i))

(defgmethod
 (tile-set-atlas-source+set-tile-animation-separation :class
  'tile-set-atlas-source :bind "set_tile_animation_separation" :hash
  1941061099)
 :void (atlas-coords vector-2i) (separation vector-2i))

(defgmethod
 (tile-set-atlas-source+get-tile-animation-separation :class
  'tile-set-atlas-source :bind "get_tile_animation_separation" :hash
  3050897911)
 vector-2i (atlas-coords vector-2i))

(defgmethod
 (tile-set-atlas-source+set-tile-animation-speed :class 'tile-set-atlas-source
  :bind "set_tile_animation_speed" :hash 2262553149)
 :void (atlas-coords vector-2i) (speed float))

(defgmethod
 (tile-set-atlas-source+get-tile-animation-speed :class 'tile-set-atlas-source
  :bind "get_tile_animation_speed" :hash 719993801)
 float (atlas-coords vector-2i))

(defgmethod
 (tile-set-atlas-source+set-tile-animation-mode :class 'tile-set-atlas-source
  :bind "set_tile_animation_mode" :hash 3192753483)
 :void (atlas-coords vector-2i)
 (mode tile-set-atlas-source+tile-animation-mode))

(defgmethod
 (tile-set-atlas-source+get-tile-animation-mode :class 'tile-set-atlas-source
  :bind "get_tile_animation_mode" :hash 4025349959)
 tile-set-atlas-source+tile-animation-mode (atlas-coords vector-2i))

(defgmethod
 (tile-set-atlas-source+set-tile-animation-frames-count :class
  'tile-set-atlas-source :bind "set_tile_animation_frames_count" :hash
  3200960707)
 :void (atlas-coords vector-2i) (frames-count int))

(defgmethod
 (tile-set-atlas-source+get-tile-animation-frames-count :class
  'tile-set-atlas-source :bind "get_tile_animation_frames_count" :hash
  2485466453)
 int (atlas-coords vector-2i))

(defgmethod
 (tile-set-atlas-source+set-tile-animation-frame-duration :class
  'tile-set-atlas-source :bind "set_tile_animation_frame_duration" :hash
  2843487787)
 :void (atlas-coords vector-2i) (frame-index int) (duration float))

(defgmethod
 (tile-set-atlas-source+get-tile-animation-frame-duration :class
  'tile-set-atlas-source :bind "get_tile_animation_frame_duration" :hash
  1802448425)
 float (atlas-coords vector-2i) (frame-index int))

(defgmethod
 (tile-set-atlas-source+get-tile-animation-total-duration :class
  'tile-set-atlas-source :bind "get_tile_animation_total_duration" :hash
  719993801)
 float (atlas-coords vector-2i))

(defgmethod
 (tile-set-atlas-source+create-alternative-tile :class 'tile-set-atlas-source
  :bind "create_alternative_tile" :hash 2226298068)
 int (atlas-coords vector-2i) (alternative-id-override int))

(defgmethod
 (tile-set-atlas-source+remove-alternative-tile :class 'tile-set-atlas-source
  :bind "remove_alternative_tile" :hash 3200960707)
 :void (atlas-coords vector-2i) (alternative-tile int))

(defgmethod
 (tile-set-atlas-source+set-alternative-tile-id :class 'tile-set-atlas-source
  :bind "set_alternative_tile_id" :hash 1499785778)
 :void (atlas-coords vector-2i) (alternative-tile int) (new-id int))

(defgmethod
 (tile-set-atlas-source+get-next-alternative-tile-id :class
  'tile-set-atlas-source :bind "get_next_alternative_tile_id" :hash 2485466453)
 int (atlas-coords vector-2i))

(defgmethod
 (tile-set-atlas-source+get-tile-data :class 'tile-set-atlas-source :bind
  "get_tile_data" :hash 3534028207)
 tile-data (atlas-coords vector-2i) (alternative-tile int))

(defgmethod
 (tile-set-atlas-source+get-atlas-grid-size :class 'tile-set-atlas-source :bind
  "get_atlas_grid_size" :hash 3690982128)
 vector-2i)

(defgmethod
 (tile-set-atlas-source+get-tile-texture-region :class 'tile-set-atlas-source
  :bind "get_tile_texture_region" :hash 241857547)
 rect-2i (atlas-coords vector-2i) (frame int))

(defgmethod
 (tile-set-atlas-source+get-runtime-texture :class 'tile-set-atlas-source :bind
  "get_runtime_texture" :hash 3635182373)
 texture-2d)

(defgmethod
 (tile-set-atlas-source+get-runtime-tile-texture-region :class
  'tile-set-atlas-source :bind "get_runtime_tile_texture_region" :hash
  104874263)
 rect-2i (atlas-coords vector-2i) (frame int))