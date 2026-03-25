(common-lisp:in-package :%godot)


(defgmethod
 (file-access+open :class 'file-access :bind "open" :hash 1247358404 :static
  common-lisp:t)
 file-access (path string) (flags file-access+mode-flags))

(defgmethod
 (file-access+open-encrypted :class 'file-access :bind "open_encrypted" :hash
  788003459 :static common-lisp:t)
 file-access (path string) (mode-flags file-access+mode-flags)
 (key packed-byte-array) (iv packed-byte-array))

(defgmethod
 (file-access+open-encrypted-with-pass :class 'file-access :bind
  "open_encrypted_with_pass" :hash 790283377 :static common-lisp:t)
 file-access (path string) (mode-flags file-access+mode-flags) (pass string))

(defgmethod
 (file-access+open-compressed :class 'file-access :bind "open_compressed" :hash
  3686439335 :static common-lisp:t)
 file-access (path string) (mode-flags file-access+mode-flags)
 (compression-mode file-access+compression-mode))

(defgmethod
 (file-access+get-open-error :class 'file-access :bind "get_open_error" :hash
  166280745 :static common-lisp:t)
 error)

(defgmethod
 (file-access+create-temp :class 'file-access :bind "create_temp" :hash
  171914364 :static common-lisp:t)
 file-access (mode-flags file-access+mode-flags) (prefix string)
 (extension string) (keep bool))

(defgmethod
 (file-access+get-file-as-bytes :class 'file-access :bind "get_file_as_bytes"
  :hash 659035735 :static common-lisp:t)
 packed-byte-array (path string))

(defgmethod
 (file-access+get-file-as-string :class 'file-access :bind "get_file_as_string"
  :hash 1703090593 :static common-lisp:t)
 string (path string))

(defgmethod
 (file-access+resize :class 'file-access :bind "resize" :hash 844576869) error
 (length int))

(defgmethod
 (file-access+flush :class 'file-access :bind "flush" :hash 3218959716) :void)

(defgmethod
 (file-access+get-path :class 'file-access :bind "get_path" :hash 201670096)
 string)

(defgmethod
 (file-access+get-path-absolute :class 'file-access :bind "get_path_absolute"
  :hash 201670096)
 string)

(defgmethod
 (file-access+is-open :class 'file-access :bind "is_open" :hash 36873697) bool)

(defgmethod
 (file-access+seek :class 'file-access :bind "seek" :hash 1286410249) :void
 (position int))

(defgmethod
 (file-access+seek-end :class 'file-access :bind "seek_end" :hash 1995695955)
 :void (position int))

(defgmethod
 (file-access+get-position :class 'file-access :bind "get_position" :hash
  3905245786)
 int)

(defgmethod
 (file-access+get-length :class 'file-access :bind "get_length" :hash
  3905245786)
 int)

(defgmethod
 (file-access+eof-reached :class 'file-access :bind "eof_reached" :hash
  36873697)
 bool)

(defgmethod
 (file-access+get-8 :class 'file-access :bind "get_8" :hash 3905245786) int)

(defgmethod
 (file-access+get-16 :class 'file-access :bind "get_16" :hash 3905245786) int)

(defgmethod
 (file-access+get-32 :class 'file-access :bind "get_32" :hash 3905245786) int)

(defgmethod
 (file-access+get-64 :class 'file-access :bind "get_64" :hash 3905245786) int)

(defgmethod
 (file-access+get-half :class 'file-access :bind "get_half" :hash 1740695150)
 float)

(defgmethod
 (file-access+get-float :class 'file-access :bind "get_float" :hash 1740695150)
 float)

(defgmethod
 (file-access+get-double :class 'file-access :bind "get_double" :hash
  1740695150)
 float)

(defgmethod
 (file-access+get-real :class 'file-access :bind "get_real" :hash 1740695150)
 float)

(defgmethod
 (file-access+get-buffer :class 'file-access :bind "get_buffer" :hash
  4131300905)
 packed-byte-array (length int))

(defgmethod
 (file-access+get-line :class 'file-access :bind "get_line" :hash 201670096)
 string)

(defgmethod
 (file-access+get-csv-line :class 'file-access :bind "get_csv_line" :hash
  2358116058)
 packed-string-array (delim string))

(defgmethod
 (file-access+get-as-text :class 'file-access :bind "get_as_text" :hash
  201670096)
 string)

(defgmethod
 (file-access+get-md5 :class 'file-access :bind "get_md5" :hash 1703090593
  :static common-lisp:t)
 string (path string))

(defgmethod
 (file-access+get-sha256 :class 'file-access :bind "get_sha256" :hash
  1703090593 :static common-lisp:t)
 string (path string))

(defgmethod
 (file-access+is-big-endian :class 'file-access :bind "is_big_endian" :hash
  36873697)
 bool)

(defgmethod
 (file-access+set-big-endian :class 'file-access :bind "set_big_endian" :hash
  2586408642)
 :void (big-endian bool))

(defgmethod
 (file-access+get-error :class 'file-access :bind "get_error" :hash 3185525595)
 error)

(defgmethod
 (file-access+get-var :class 'file-access :bind "get_var" :hash 189129690)
 variant (allow-objects bool))

(defgmethod
 (file-access+store-8 :class 'file-access :bind "store_8" :hash 3067735520)
 bool (value int))

(defgmethod
 (file-access+store-16 :class 'file-access :bind "store_16" :hash 3067735520)
 bool (value int))

(defgmethod
 (file-access+store-32 :class 'file-access :bind "store_32" :hash 3067735520)
 bool (value int))

(defgmethod
 (file-access+store-64 :class 'file-access :bind "store_64" :hash 3067735520)
 bool (value int))

(defgmethod
 (file-access+store-half :class 'file-access :bind "store_half" :hash
  330693286)
 bool (value float))

(defgmethod
 (file-access+store-float :class 'file-access :bind "store_float" :hash
  330693286)
 bool (value float))

(defgmethod
 (file-access+store-double :class 'file-access :bind "store_double" :hash
  330693286)
 bool (value float))

(defgmethod
 (file-access+store-real :class 'file-access :bind "store_real" :hash
  330693286)
 bool (value float))

(defgmethod
 (file-access+store-buffer :class 'file-access :bind "store_buffer" :hash
  114037665)
 bool (buffer packed-byte-array))

(defgmethod
 (file-access+store-line :class 'file-access :bind "store_line" :hash
  2323990056)
 bool (line string))

(defgmethod
 (file-access+store-csv-line :class 'file-access :bind "store_csv_line" :hash
  1611473434)
 bool (values packed-string-array) (delim string))

(defgmethod
 (file-access+store-string :class 'file-access :bind "store_string" :hash
  2323990056)
 bool (string string))

(defgmethod
 (file-access+store-var :class 'file-access :bind "store_var" :hash 117357437)
 bool (value variant) (full-objects bool))

(defgmethod
 (file-access+store-pascal-string :class 'file-access :bind
  "store_pascal_string" :hash 2323990056)
 bool (string string))

(defgmethod
 (file-access+get-pascal-string :class 'file-access :bind "get_pascal_string"
  :hash 2841200299)
 string)

(defgmethod
 (file-access+close :class 'file-access :bind "close" :hash 3218959716) :void)

(defgmethod
 (file-access+file-exists :class 'file-access :bind "file_exists" :hash
  2323990056 :static common-lisp:t)
 bool (path string))

(defgmethod
 (file-access+get-modified-time :class 'file-access :bind "get_modified_time"
  :hash 1597066294 :static common-lisp:t)
 int (file string))

(defgmethod
 (file-access+get-access-time :class 'file-access :bind "get_access_time" :hash
  1597066294 :static common-lisp:t)
 int (file string))

(defgmethod
 (file-access+get-size :class 'file-access :bind "get_size" :hash 1597066294
  :static common-lisp:t)
 int (file string))

(defgmethod
 (file-access+get-unix-permissions :class 'file-access :bind
  "get_unix_permissions" :hash 524341837 :static common-lisp:t)
 file-access+unix-permission-flags (file string))

(defgmethod
 (file-access+set-unix-permissions :class 'file-access :bind
  "set_unix_permissions" :hash 846038644 :static common-lisp:t)
 error (file string) (permissions file-access+unix-permission-flags))

(defgmethod
 (file-access+get-hidden-attribute :class 'file-access :bind
  "get_hidden_attribute" :hash 2323990056 :static common-lisp:t)
 bool (file string))

(defgmethod
 (file-access+set-hidden-attribute :class 'file-access :bind
  "set_hidden_attribute" :hash 2892558115 :static common-lisp:t)
 error (file string) (hidden bool))

(defgmethod
 (file-access+set-read-only-attribute :class 'file-access :bind
  "set_read_only_attribute" :hash 2892558115 :static common-lisp:t)
 error (file string) (ro bool))

(defgmethod
 (file-access+get-read-only-attribute :class 'file-access :bind
  "get_read_only_attribute" :hash 2323990056 :static common-lisp:t)
 bool (file string))

(defgmethod
 (file-access+get-extended-attribute :class 'file-access :bind
  "get_extended_attribute" :hash 955893464 :static common-lisp:t)
 packed-byte-array (file string) (attribute-name string))

(defgmethod
 (file-access+get-extended-attribute-string :class 'file-access :bind
  "get_extended_attribute_string" :hash 1218461987 :static common-lisp:t)
 string (file string) (attribute-name string))

(defgmethod
 (file-access+set-extended-attribute :class 'file-access :bind
  "set_extended_attribute" :hash 2643421469 :static common-lisp:t)
 error (file string) (attribute-name string) (data packed-byte-array))

(defgmethod
 (file-access+set-extended-attribute-string :class 'file-access :bind
  "set_extended_attribute_string" :hash 699024349 :static common-lisp:t)
 error (file string) (attribute-name string) (data string))

(defgmethod
 (file-access+remove-extended-attribute :class 'file-access :bind
  "remove_extended_attribute" :hash 852856452 :static common-lisp:t)
 error (file string) (attribute-name string))

(defgmethod
 (file-access+get-extended-attributes-list :class 'file-access :bind
  "get_extended_attributes_list" :hash 3538744774 :static common-lisp:t)
 packed-string-array (file string))