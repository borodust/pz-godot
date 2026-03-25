(common-lisp:in-package :%godot)


(defgmethod
 (config-file+set-value :class 'config-file :bind "set_value" :hash 2504492430)
 :void (section string) (key string) (value variant))

(defgmethod
 (config-file+get-value :class 'config-file :bind "get_value" :hash 89809366)
 variant (section string) (key string) (default variant))

(defgmethod
 (config-file+has-section :class 'config-file :bind "has_section" :hash
  3927539163)
 bool (section string))

(defgmethod
 (config-file+has-section-key :class 'config-file :bind "has_section_key" :hash
  820780508)
 bool (section string) (key string))

(defgmethod
 (config-file+get-sections :class 'config-file :bind "get_sections" :hash
  1139954409)
 packed-string-array)

(defgmethod
 (config-file+get-section-keys :class 'config-file :bind "get_section_keys"
  :hash 4291131558)
 packed-string-array (section string))

(defgmethod
 (config-file+erase-section :class 'config-file :bind "erase_section" :hash
  83702148)
 :void (section string))

(defgmethod
 (config-file+erase-section-key :class 'config-file :bind "erase_section_key"
  :hash 3186203200)
 :void (section string) (key string))

(defgmethod (config-file+load :class 'config-file :bind "load" :hash 166001499)
 error (path string))

(defgmethod
 (config-file+parse :class 'config-file :bind "parse" :hash 166001499) error
 (data string))

(defgmethod (config-file+save :class 'config-file :bind "save" :hash 166001499)
 error (path string))

(defgmethod
 (config-file+encode-to-text :class 'config-file :bind "encode_to_text" :hash
  201670096)
 string)

(defgmethod
 (config-file+load-encrypted :class 'config-file :bind "load_encrypted" :hash
  887037711)
 error (path string) (key packed-byte-array))

(defgmethod
 (config-file+load-encrypted-pass :class 'config-file :bind
  "load_encrypted_pass" :hash 852856452)
 error (path string) (password string))

(defgmethod
 (config-file+save-encrypted :class 'config-file :bind "save_encrypted" :hash
  887037711)
 error (path string) (key packed-byte-array))

(defgmethod
 (config-file+save-encrypted-pass :class 'config-file :bind
  "save_encrypted_pass" :hash 852856452)
 error (path string) (password string))

(defgmethod
 (config-file+clear :class 'config-file :bind "clear" :hash 3218959716) :void)