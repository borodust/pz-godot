(common-lisp:in-package :%godot)


(defgmethod
 (stream-peer+put-data :class 'stream-peer :bind "put_data" :hash 680677267)
 error (data packed-byte-array))

(defgmethod
 (stream-peer+put-partial-data :class 'stream-peer :bind "put_partial_data"
  :hash 2934048347)
 array (data packed-byte-array))

(defgmethod
 (stream-peer+get-data :class 'stream-peer :bind "get_data" :hash 1171824711)
 array (bytes int))

(defgmethod
 (stream-peer+get-partial-data :class 'stream-peer :bind "get_partial_data"
  :hash 1171824711)
 array (bytes int))

(defgmethod
 (stream-peer+get-available-bytes :class 'stream-peer :bind
  "get_available_bytes" :hash 3905245786)
 int)

(defgmethod
 (stream-peer+set-big-endian :class 'stream-peer :bind "set_big_endian" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (stream-peer+is-big-endian-enabled :class 'stream-peer :bind
  "is_big_endian_enabled" :hash 36873697)
 bool)

(defgmethod
 (stream-peer+put-8 :class 'stream-peer :bind "put_8" :hash 1286410249) :void
 (value int))

(defgmethod
 (stream-peer+put-u8 :class 'stream-peer :bind "put_u8" :hash 1286410249) :void
 (value int))

(defgmethod
 (stream-peer+put-16 :class 'stream-peer :bind "put_16" :hash 1286410249) :void
 (value int))

(defgmethod
 (stream-peer+put-u16 :class 'stream-peer :bind "put_u16" :hash 1286410249)
 :void (value int))

(defgmethod
 (stream-peer+put-32 :class 'stream-peer :bind "put_32" :hash 1286410249) :void
 (value int))

(defgmethod
 (stream-peer+put-u32 :class 'stream-peer :bind "put_u32" :hash 1286410249)
 :void (value int))

(defgmethod
 (stream-peer+put-64 :class 'stream-peer :bind "put_64" :hash 1286410249) :void
 (value int))

(defgmethod
 (stream-peer+put-u64 :class 'stream-peer :bind "put_u64" :hash 1286410249)
 :void (value int))

(defgmethod
 (stream-peer+put-half :class 'stream-peer :bind "put_half" :hash 373806689)
 :void (value float))

(defgmethod
 (stream-peer+put-float :class 'stream-peer :bind "put_float" :hash 373806689)
 :void (value float))

(defgmethod
 (stream-peer+put-double :class 'stream-peer :bind "put_double" :hash
  373806689)
 :void (value float))

(defgmethod
 (stream-peer+put-string :class 'stream-peer :bind "put_string" :hash 83702148)
 :void (value string))

(defgmethod
 (stream-peer+put-utf8-string :class 'stream-peer :bind "put_utf8_string" :hash
  83702148)
 :void (value string))

(defgmethod
 (stream-peer+put-var :class 'stream-peer :bind "put_var" :hash 738511890)
 :void (value variant) (full-objects bool))

(defgmethod
 (stream-peer+get-8 :class 'stream-peer :bind "get_8" :hash 2455072627) int)

(defgmethod
 (stream-peer+get-u8 :class 'stream-peer :bind "get_u8" :hash 2455072627) int)

(defgmethod
 (stream-peer+get-16 :class 'stream-peer :bind "get_16" :hash 2455072627) int)

(defgmethod
 (stream-peer+get-u16 :class 'stream-peer :bind "get_u16" :hash 2455072627) int)

(defgmethod
 (stream-peer+get-32 :class 'stream-peer :bind "get_32" :hash 2455072627) int)

(defgmethod
 (stream-peer+get-u32 :class 'stream-peer :bind "get_u32" :hash 2455072627) int)

(defgmethod
 (stream-peer+get-64 :class 'stream-peer :bind "get_64" :hash 2455072627) int)

(defgmethod
 (stream-peer+get-u64 :class 'stream-peer :bind "get_u64" :hash 2455072627) int)

(defgmethod
 (stream-peer+get-half :class 'stream-peer :bind "get_half" :hash 191475506)
 float)

(defgmethod
 (stream-peer+get-float :class 'stream-peer :bind "get_float" :hash 191475506)
 float)

(defgmethod
 (stream-peer+get-double :class 'stream-peer :bind "get_double" :hash
  191475506)
 float)

(defgmethod
 (stream-peer+get-string :class 'stream-peer :bind "get_string" :hash
  2309358862)
 string (bytes int))

(defgmethod
 (stream-peer+get-utf8-string :class 'stream-peer :bind "get_utf8_string" :hash
  2309358862)
 string (bytes int))

(defgmethod
 (stream-peer+get-var :class 'stream-peer :bind "get_var" :hash 3442865206)
 variant (allow-objects bool))