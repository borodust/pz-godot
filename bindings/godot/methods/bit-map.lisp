(common-lisp:in-package :%godot)


(defgmethod (bit-map+create :class 'bit-map :bind "create" :hash 1130785943)
 :void (size vector-2i))

(defgmethod
 (bit-map+create-from-image-alpha :class 'bit-map :bind
  "create_from_image_alpha" :hash 106271684)
 :void (image image) (threshold float))

(defgmethod
 (bit-map+set-bitv :class 'bit-map :bind "set_bitv" :hash 4153096796) :void
 (position vector-2i) (bit bool))

(defgmethod (bit-map+set-bit :class 'bit-map :bind "set_bit" :hash 1383440665)
 :void (x int) (y int) (bit bool))

(defgmethod
 (bit-map+get-bitv :class 'bit-map :bind "get_bitv" :hash 3900751641) bool
 (position vector-2i))

(defgmethod (bit-map+get-bit :class 'bit-map :bind "get_bit" :hash 2522259332)
 bool (x int) (y int))

(defgmethod
 (bit-map+set-bit-rect :class 'bit-map :bind "set_bit_rect" :hash 472162941)
 :void (rect rect-2i) (bit bool))

(defgmethod
 (bit-map+get-true-bit-count :class 'bit-map :bind "get_true_bit_count" :hash
  3905245786)
 int)

(defgmethod
 (bit-map+get-size :class 'bit-map :bind "get_size" :hash 3690982128) vector-2i)

(defgmethod (bit-map+resize :class 'bit-map :bind "resize" :hash 1130785943)
 :void (new-size vector-2i))

(defgmethod
 (bit-map+grow-mask :class 'bit-map :bind "grow_mask" :hash 3317281434) :void
 (pixels int) (rect rect-2i))

(defgmethod
 (bit-map+convert-to-image :class 'bit-map :bind "convert_to_image" :hash
  4190603485)
 image)

(defgmethod
 (bit-map+opaque-to-polygons :class 'bit-map :bind "opaque_to_polygons" :hash
  48478126)
 array (rect rect-2i) (epsilon float))