(common-lisp:in-package :%godot)


(defgmethod
 (gltfbuffer-view+load-buffer-view-data :class 'gltfbuffer-view :bind
  "load_buffer_view_data" :hash 3945446907)
 packed-byte-array (state gltfstate))

(defgmethod
 (gltfbuffer-view+from-dictionary :class 'gltfbuffer-view :bind
  "from_dictionary" :hash 2594413512 :static common-lisp:t)
 gltfbuffer-view (dictionary dictionary))

(defgmethod
 (gltfbuffer-view+to-dictionary :class 'gltfbuffer-view :bind "to_dictionary"
  :hash 3102165223)
 dictionary)

(defgmethod
 (gltfbuffer-view+get-buffer :class 'gltfbuffer-view :bind "get_buffer" :hash
  3905245786)
 int)

(defgmethod
 (gltfbuffer-view+set-buffer :class 'gltfbuffer-view :bind "set_buffer" :hash
  1286410249)
 :void (buffer int))

(defgmethod
 (gltfbuffer-view+get-byte-offset :class 'gltfbuffer-view :bind
  "get_byte_offset" :hash 3905245786)
 int)

(defgmethod
 (gltfbuffer-view+set-byte-offset :class 'gltfbuffer-view :bind
  "set_byte_offset" :hash 1286410249)
 :void (byte-offset int))

(defgmethod
 (gltfbuffer-view+get-byte-length :class 'gltfbuffer-view :bind
  "get_byte_length" :hash 3905245786)
 int)

(defgmethod
 (gltfbuffer-view+set-byte-length :class 'gltfbuffer-view :bind
  "set_byte_length" :hash 1286410249)
 :void (byte-length int))

(defgmethod
 (gltfbuffer-view+get-byte-stride :class 'gltfbuffer-view :bind
  "get_byte_stride" :hash 3905245786)
 int)

(defgmethod
 (gltfbuffer-view+set-byte-stride :class 'gltfbuffer-view :bind
  "set_byte_stride" :hash 1286410249)
 :void (byte-stride int))

(defgmethod
 (gltfbuffer-view+get-indices :class 'gltfbuffer-view :bind "get_indices" :hash
  36873697)
 bool)

(defgmethod
 (gltfbuffer-view+set-indices :class 'gltfbuffer-view :bind "set_indices" :hash
  2586408642)
 :void (indices bool))

(defgmethod
 (gltfbuffer-view+get-vertex-attributes :class 'gltfbuffer-view :bind
  "get_vertex_attributes" :hash 36873697)
 bool)

(defgmethod
 (gltfbuffer-view+set-vertex-attributes :class 'gltfbuffer-view :bind
  "set_vertex_attributes" :hash 2586408642)
 :void (is-attributes bool))