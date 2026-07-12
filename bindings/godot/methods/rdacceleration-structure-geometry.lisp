(common-lisp:in-package :%godot)


(defgmethod
 (rdacceleration-structure-geometry+set-flags :class
  'rdacceleration-structure-geometry :bind "set_flags" :hash 1046628555)
 :void (p-member rendering-device+acceleration-structure-geometry-flag-bits))

(defgmethod
 (rdacceleration-structure-geometry+get-flags :class
  'rdacceleration-structure-geometry :bind "get_flags" :hash 1694887119)
 rendering-device+acceleration-structure-geometry-flag-bits)

(defgmethod
 (rdacceleration-structure-geometry+set-vertex-buffer :class
  'rdacceleration-structure-geometry :bind "set_vertex_buffer" :hash
  2722037293)
 :void (p-member rid))

(defgmethod
 (rdacceleration-structure-geometry+get-vertex-buffer :class
  'rdacceleration-structure-geometry :bind "get_vertex_buffer" :hash
  2944877500)
 rid)

(defgmethod
 (rdacceleration-structure-geometry+set-vertex-offset :class
  'rdacceleration-structure-geometry :bind "set_vertex_offset" :hash
  1286410249)
 :void (p-member int))

(defgmethod
 (rdacceleration-structure-geometry+get-vertex-offset :class
  'rdacceleration-structure-geometry :bind "get_vertex_offset" :hash
  3905245786)
 int)

(defgmethod
 (rdacceleration-structure-geometry+set-vertex-stride :class
  'rdacceleration-structure-geometry :bind "set_vertex_stride" :hash
  1286410249)
 :void (p-member int))

(defgmethod
 (rdacceleration-structure-geometry+get-vertex-stride :class
  'rdacceleration-structure-geometry :bind "get_vertex_stride" :hash
  3905245786)
 int)

(defgmethod
 (rdacceleration-structure-geometry+set-vertex-count :class
  'rdacceleration-structure-geometry :bind "set_vertex_count" :hash 1286410249)
 :void (p-member int))

(defgmethod
 (rdacceleration-structure-geometry+get-vertex-count :class
  'rdacceleration-structure-geometry :bind "get_vertex_count" :hash 3905245786)
 int)

(defgmethod
 (rdacceleration-structure-geometry+set-vertex-format :class
  'rdacceleration-structure-geometry :bind "set_vertex_format" :hash 565531219)
 :void (p-member rendering-device+data-format))

(defgmethod
 (rdacceleration-structure-geometry+get-vertex-format :class
  'rdacceleration-structure-geometry :bind "get_vertex_format" :hash
  2235804183)
 rendering-device+data-format)

(defgmethod
 (rdacceleration-structure-geometry+set-index-buffer :class
  'rdacceleration-structure-geometry :bind "set_index_buffer" :hash 2722037293)
 :void (p-member rid))

(defgmethod
 (rdacceleration-structure-geometry+get-index-buffer :class
  'rdacceleration-structure-geometry :bind "get_index_buffer" :hash 2944877500)
 rid)

(defgmethod
 (rdacceleration-structure-geometry+set-index-offset :class
  'rdacceleration-structure-geometry :bind "set_index_offset" :hash 1286410249)
 :void (p-member int))

(defgmethod
 (rdacceleration-structure-geometry+get-index-offset :class
  'rdacceleration-structure-geometry :bind "get_index_offset" :hash 3905245786)
 int)

(defgmethod
 (rdacceleration-structure-geometry+set-index-count :class
  'rdacceleration-structure-geometry :bind "set_index_count" :hash 1286410249)
 :void (p-member int))

(defgmethod
 (rdacceleration-structure-geometry+get-index-count :class
  'rdacceleration-structure-geometry :bind "get_index_count" :hash 3905245786)
 int)