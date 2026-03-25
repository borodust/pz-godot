(common-lisp:in-package :%godot)


(defgmethod
 (packed-byte-array+get :class 'packed-byte-array :bind "get" :hash 4103005248)
 int (index int))

(defgmethod
 (packed-byte-array+set :class 'packed-byte-array :bind "set" :hash 3638975848)
 :void (index int) (value int))

(defgmethod
 (packed-byte-array+size :class 'packed-byte-array :bind "size" :hash
  3173160232)
 int)

(defgmethod
 (packed-byte-array+is-empty :class 'packed-byte-array :bind "is_empty" :hash
  3918633141)
 bool)

(defgmethod
 (packed-byte-array+push-back :class 'packed-byte-array :bind "push_back" :hash
  694024632)
 bool (value int))

(defgmethod
 (packed-byte-array+append :class 'packed-byte-array :bind "append" :hash
  694024632)
 bool (value int))

(defgmethod
 (packed-byte-array+append-array :class 'packed-byte-array :bind "append_array"
  :hash 791097111)
 :void (array packed-byte-array))

(defgmethod
 (packed-byte-array+remove-at :class 'packed-byte-array :bind "remove_at" :hash
  2823966027)
 :void (index int))

(defgmethod
 (packed-byte-array+insert :class 'packed-byte-array :bind "insert" :hash
  1487112728)
 int (at-index int) (value int))

(defgmethod
 (packed-byte-array+fill :class 'packed-byte-array :bind "fill" :hash
  2823966027)
 :void (value int))

(defgmethod
 (packed-byte-array+resize :class 'packed-byte-array :bind "resize" :hash
  848867239)
 int (new-size int))

(defgmethod
 (packed-byte-array+clear :class 'packed-byte-array :bind "clear" :hash
  3218959716)
 :void)

(defgmethod
 (packed-byte-array+has :class 'packed-byte-array :bind "has" :hash 931488181)
 bool (value int))

(defgmethod
 (packed-byte-array+reverse :class 'packed-byte-array :bind "reverse" :hash
  3218959716)
 :void)

(defgmethod
 (packed-byte-array+slice :class 'packed-byte-array :bind "slice" :hash
  2278869132)
 packed-byte-array (begin int) (end int))

(defgmethod
 (packed-byte-array+sort :class 'packed-byte-array :bind "sort" :hash
  3218959716)
 :void)

(defgmethod
 (packed-byte-array+bsearch :class 'packed-byte-array :bind "bsearch" :hash
  954237325)
 int (value int) (before bool))

(defgmethod
 (packed-byte-array+duplicate :class 'packed-byte-array :bind "duplicate" :hash
  247621236)
 packed-byte-array)

(defgmethod
 (packed-byte-array+find :class 'packed-byte-array :bind "find" :hash
  2984303840)
 int (value int) (from int))

(defgmethod
 (packed-byte-array+rfind :class 'packed-byte-array :bind "rfind" :hash
  2984303840)
 int (value int) (from int))

(defgmethod
 (packed-byte-array+count :class 'packed-byte-array :bind "count" :hash
  4103005248)
 int (value int))

(defgmethod
 (packed-byte-array+erase :class 'packed-byte-array :bind "erase" :hash
  694024632)
 bool (value int))

(defgmethod
 (packed-byte-array+get-string-from-ascii :class 'packed-byte-array :bind
  "get_string_from_ascii" :hash 3942272618)
 string)

(defgmethod
 (packed-byte-array+get-string-from-utf8 :class 'packed-byte-array :bind
  "get_string_from_utf8" :hash 3942272618)
 string)

(defgmethod
 (packed-byte-array+get-string-from-utf16 :class 'packed-byte-array :bind
  "get_string_from_utf16" :hash 3942272618)
 string)

(defgmethod
 (packed-byte-array+get-string-from-utf32 :class 'packed-byte-array :bind
  "get_string_from_utf32" :hash 3942272618)
 string)

(defgmethod
 (packed-byte-array+get-string-from-wchar :class 'packed-byte-array :bind
  "get_string_from_wchar" :hash 3942272618)
 string)

(defgmethod
 (packed-byte-array+get-string-from-multibyte-char :class 'packed-byte-array
  :bind "get_string_from_multibyte_char" :hash 3134094431)
 string (encoding string))

(defgmethod
 (packed-byte-array+hex-encode :class 'packed-byte-array :bind "hex_encode"
  :hash 3942272618)
 string)

(defgmethod
 (packed-byte-array+compress :class 'packed-byte-array :bind "compress" :hash
  1845905913)
 packed-byte-array (compression-mode int))

(defgmethod
 (packed-byte-array+decompress :class 'packed-byte-array :bind "decompress"
  :hash 2278869132)
 packed-byte-array (buffer-size int) (compression-mode int))

(defgmethod
 (packed-byte-array+decompress-dynamic :class 'packed-byte-array :bind
  "decompress_dynamic" :hash 2278869132)
 packed-byte-array (max-output-size int) (compression-mode int))

(defgmethod
 (packed-byte-array+decode-u8 :class 'packed-byte-array :bind "decode_u8" :hash
  4103005248)
 int (byte-offset int))

(defgmethod
 (packed-byte-array+decode-s8 :class 'packed-byte-array :bind "decode_s8" :hash
  4103005248)
 int (byte-offset int))

(defgmethod
 (packed-byte-array+decode-u16 :class 'packed-byte-array :bind "decode_u16"
  :hash 4103005248)
 int (byte-offset int))

(defgmethod
 (packed-byte-array+decode-s16 :class 'packed-byte-array :bind "decode_s16"
  :hash 4103005248)
 int (byte-offset int))

(defgmethod
 (packed-byte-array+decode-u32 :class 'packed-byte-array :bind "decode_u32"
  :hash 4103005248)
 int (byte-offset int))

(defgmethod
 (packed-byte-array+decode-s32 :class 'packed-byte-array :bind "decode_s32"
  :hash 4103005248)
 int (byte-offset int))

(defgmethod
 (packed-byte-array+decode-u64 :class 'packed-byte-array :bind "decode_u64"
  :hash 4103005248)
 int (byte-offset int))

(defgmethod
 (packed-byte-array+decode-s64 :class 'packed-byte-array :bind "decode_s64"
  :hash 4103005248)
 int (byte-offset int))

(defgmethod
 (packed-byte-array+decode-half :class 'packed-byte-array :bind "decode_half"
  :hash 1401583798)
 float (byte-offset int))

(defgmethod
 (packed-byte-array+decode-float :class 'packed-byte-array :bind "decode_float"
  :hash 1401583798)
 float (byte-offset int))

(defgmethod
 (packed-byte-array+decode-double :class 'packed-byte-array :bind
  "decode_double" :hash 1401583798)
 float (byte-offset int))

(defgmethod
 (packed-byte-array+has-encoded-var :class 'packed-byte-array :bind
  "has_encoded_var" :hash 2914632957)
 bool (byte-offset int) (allow-objects bool))

(defgmethod
 (packed-byte-array+decode-var :class 'packed-byte-array :bind "decode_var"
  :hash 1740420038)
 variant (byte-offset int) (allow-objects bool))

(defgmethod
 (packed-byte-array+decode-var-size :class 'packed-byte-array :bind
  "decode_var_size" :hash 954237325)
 int (byte-offset int) (allow-objects bool))

(defgmethod
 (packed-byte-array+to-int32-array :class 'packed-byte-array :bind
  "to_int32_array" :hash 3158844420)
 packed-int-32array)

(defgmethod
 (packed-byte-array+to-int64-array :class 'packed-byte-array :bind
  "to_int64_array" :hash 1961294120)
 packed-int-64array)

(defgmethod
 (packed-byte-array+to-float32-array :class 'packed-byte-array :bind
  "to_float32_array" :hash 3575107827)
 packed-float-32array)

(defgmethod
 (packed-byte-array+to-float64-array :class 'packed-byte-array :bind
  "to_float64_array" :hash 1627308337)
 packed-float-64array)

(defgmethod
 (packed-byte-array+to-vector2-array :class 'packed-byte-array :bind
  "to_vector2_array" :hash 1660374357)
 packed-vector-2array)

(defgmethod
 (packed-byte-array+to-vector3-array :class 'packed-byte-array :bind
  "to_vector3_array" :hash 4171207452)
 packed-vector-3array)

(defgmethod
 (packed-byte-array+to-vector4-array :class 'packed-byte-array :bind
  "to_vector4_array" :hash 146203628)
 packed-vector-4array)

(defgmethod
 (packed-byte-array+to-color-array :class 'packed-byte-array :bind
  "to_color_array" :hash 3072026941)
 packed-color-array)

(defgmethod
 (packed-byte-array+bswap16 :class 'packed-byte-array :bind "bswap16" :hash
  3638975848)
 :void (offset int) (count int))

(defgmethod
 (packed-byte-array+bswap32 :class 'packed-byte-array :bind "bswap32" :hash
  3638975848)
 :void (offset int) (count int))

(defgmethod
 (packed-byte-array+bswap64 :class 'packed-byte-array :bind "bswap64" :hash
  3638975848)
 :void (offset int) (count int))

(defgmethod
 (packed-byte-array+encode-u8 :class 'packed-byte-array :bind "encode_u8" :hash
  3638975848)
 :void (byte-offset int) (value int))

(defgmethod
 (packed-byte-array+encode-s8 :class 'packed-byte-array :bind "encode_s8" :hash
  3638975848)
 :void (byte-offset int) (value int))

(defgmethod
 (packed-byte-array+encode-u16 :class 'packed-byte-array :bind "encode_u16"
  :hash 3638975848)
 :void (byte-offset int) (value int))

(defgmethod
 (packed-byte-array+encode-s16 :class 'packed-byte-array :bind "encode_s16"
  :hash 3638975848)
 :void (byte-offset int) (value int))

(defgmethod
 (packed-byte-array+encode-u32 :class 'packed-byte-array :bind "encode_u32"
  :hash 3638975848)
 :void (byte-offset int) (value int))

(defgmethod
 (packed-byte-array+encode-s32 :class 'packed-byte-array :bind "encode_s32"
  :hash 3638975848)
 :void (byte-offset int) (value int))

(defgmethod
 (packed-byte-array+encode-u64 :class 'packed-byte-array :bind "encode_u64"
  :hash 3638975848)
 :void (byte-offset int) (value int))

(defgmethod
 (packed-byte-array+encode-s64 :class 'packed-byte-array :bind "encode_s64"
  :hash 3638975848)
 :void (byte-offset int) (value int))

(defgmethod
 (packed-byte-array+encode-half :class 'packed-byte-array :bind "encode_half"
  :hash 1113000516)
 :void (byte-offset int) (value float))

(defgmethod
 (packed-byte-array+encode-float :class 'packed-byte-array :bind "encode_float"
  :hash 1113000516)
 :void (byte-offset int) (value float))

(defgmethod
 (packed-byte-array+encode-double :class 'packed-byte-array :bind
  "encode_double" :hash 1113000516)
 :void (byte-offset int) (value float))

(defgmethod
 (packed-byte-array+encode-var :class 'packed-byte-array :bind "encode_var"
  :hash 2604460497)
 int (byte-offset int) (value variant) (allow-objects bool))