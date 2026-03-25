(common-lisp:in-package :%godot)


(defgmethod
 (marshalls+variant-to-base64 :class 'marshalls :bind "variant_to_base64" :hash
  3876248563)
 string (variant variant) (full-objects bool))

(defgmethod
 (marshalls+base64-to-variant :class 'marshalls :bind "base64_to_variant" :hash
  218087648)
 variant (base64-str string) (allow-objects bool))

(defgmethod
 (marshalls+raw-to-base64 :class 'marshalls :bind "raw_to_base64" :hash
  3999417757)
 string (array packed-byte-array))

(defgmethod
 (marshalls+base64-to-raw :class 'marshalls :bind "base64_to_raw" :hash
  659035735)
 packed-byte-array (base64-str string))

(defgmethod
 (marshalls+utf8-to-base64 :class 'marshalls :bind "utf8_to_base64" :hash
  1703090593)
 string (utf8-str string))

(defgmethod
 (marshalls+base64-to-utf8 :class 'marshalls :bind "base64_to_utf8" :hash
  1703090593)
 string (base64-str string))