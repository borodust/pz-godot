(common-lisp:in-package :%godot)


(defgmethod
 (gltfaccessor+from-dictionary :class 'gltfaccessor :bind "from_dictionary"
  :hash 3495091019 :static common-lisp:t)
 gltfaccessor (dictionary dictionary))

(defgmethod
 (gltfaccessor+to-dictionary :class 'gltfaccessor :bind "to_dictionary" :hash
  3102165223)
 dictionary)

(defgmethod
 (gltfaccessor+get-buffer-view :class 'gltfaccessor :bind "get_buffer_view"
  :hash 3905245786)
 int)

(defgmethod
 (gltfaccessor+set-buffer-view :class 'gltfaccessor :bind "set_buffer_view"
  :hash 1286410249)
 :void (buffer-view int))

(defgmethod
 (gltfaccessor+get-byte-offset :class 'gltfaccessor :bind "get_byte_offset"
  :hash 3905245786)
 int)

(defgmethod
 (gltfaccessor+set-byte-offset :class 'gltfaccessor :bind "set_byte_offset"
  :hash 1286410249)
 :void (byte-offset int))

(defgmethod
 (gltfaccessor+get-component-type :class 'gltfaccessor :bind
  "get_component_type" :hash 852227802)
 gltfaccessor+gltfcomponent-type)

(defgmethod
 (gltfaccessor+set-component-type :class 'gltfaccessor :bind
  "set_component_type" :hash 1780020221)
 :void (component-type gltfaccessor+gltfcomponent-type))

(defgmethod
 (gltfaccessor+get-normalized :class 'gltfaccessor :bind "get_normalized" :hash
  36873697)
 bool)

(defgmethod
 (gltfaccessor+set-normalized :class 'gltfaccessor :bind "set_normalized" :hash
  2586408642)
 :void (normalized bool))

(defgmethod
 (gltfaccessor+get-count :class 'gltfaccessor :bind "get_count" :hash
  3905245786)
 int)

(defgmethod
 (gltfaccessor+set-count :class 'gltfaccessor :bind "set_count" :hash
  1286410249)
 :void (count int))

(defgmethod
 (gltfaccessor+get-accessor-type :class 'gltfaccessor :bind "get_accessor_type"
  :hash 1998183368)
 gltfaccessor+gltfaccessor-type)

(defgmethod
 (gltfaccessor+set-accessor-type :class 'gltfaccessor :bind "set_accessor_type"
  :hash 2347728198)
 :void (accessor-type gltfaccessor+gltfaccessor-type))

(defgmethod
 (gltfaccessor+get-type :class 'gltfaccessor :bind "get_type" :hash 3905245786)
 int)

(defgmethod
 (gltfaccessor+set-type :class 'gltfaccessor :bind "set_type" :hash 1286410249)
 :void (type int))

(defgmethod
 (gltfaccessor+get-min :class 'gltfaccessor :bind "get_min" :hash 547233126)
 packed-float-64array)

(defgmethod
 (gltfaccessor+set-min :class 'gltfaccessor :bind "set_min" :hash 2576592201)
 :void (min packed-float-64array))

(defgmethod
 (gltfaccessor+get-max :class 'gltfaccessor :bind "get_max" :hash 547233126)
 packed-float-64array)

(defgmethod
 (gltfaccessor+set-max :class 'gltfaccessor :bind "set_max" :hash 2576592201)
 :void (max packed-float-64array))

(defgmethod
 (gltfaccessor+get-sparse-count :class 'gltfaccessor :bind "get_sparse_count"
  :hash 3905245786)
 int)

(defgmethod
 (gltfaccessor+set-sparse-count :class 'gltfaccessor :bind "set_sparse_count"
  :hash 1286410249)
 :void (sparse-count int))

(defgmethod
 (gltfaccessor+get-sparse-indices-buffer-view :class 'gltfaccessor :bind
  "get_sparse_indices_buffer_view" :hash 3905245786)
 int)

(defgmethod
 (gltfaccessor+set-sparse-indices-buffer-view :class 'gltfaccessor :bind
  "set_sparse_indices_buffer_view" :hash 1286410249)
 :void (sparse-indices-buffer-view int))

(defgmethod
 (gltfaccessor+get-sparse-indices-byte-offset :class 'gltfaccessor :bind
  "get_sparse_indices_byte_offset" :hash 3905245786)
 int)

(defgmethod
 (gltfaccessor+set-sparse-indices-byte-offset :class 'gltfaccessor :bind
  "set_sparse_indices_byte_offset" :hash 1286410249)
 :void (sparse-indices-byte-offset int))

(defgmethod
 (gltfaccessor+get-sparse-indices-component-type :class 'gltfaccessor :bind
  "get_sparse_indices_component_type" :hash 852227802)
 gltfaccessor+gltfcomponent-type)

(defgmethod
 (gltfaccessor+set-sparse-indices-component-type :class 'gltfaccessor :bind
  "set_sparse_indices_component_type" :hash 1780020221)
 :void (sparse-indices-component-type gltfaccessor+gltfcomponent-type))

(defgmethod
 (gltfaccessor+get-sparse-values-buffer-view :class 'gltfaccessor :bind
  "get_sparse_values_buffer_view" :hash 3905245786)
 int)

(defgmethod
 (gltfaccessor+set-sparse-values-buffer-view :class 'gltfaccessor :bind
  "set_sparse_values_buffer_view" :hash 1286410249)
 :void (sparse-values-buffer-view int))

(defgmethod
 (gltfaccessor+get-sparse-values-byte-offset :class 'gltfaccessor :bind
  "get_sparse_values_byte_offset" :hash 3905245786)
 int)

(defgmethod
 (gltfaccessor+set-sparse-values-byte-offset :class 'gltfaccessor :bind
  "set_sparse_values_byte_offset" :hash 1286410249)
 :void (sparse-values-byte-offset int))