(common-lisp:in-package :%godot)


(defgmethod
 (crypto+generate-random-bytes :class 'crypto :bind "generate_random_bytes"
  :hash 47165747)
 packed-byte-array (size int))

(defgmethod
 (crypto+generate-rsa :class 'crypto :bind "generate_rsa" :hash 1237515462)
 crypto-key (size int))

(defgmethod
 (crypto+generate-self-signed-certificate :class 'crypto :bind
  "generate_self_signed_certificate" :hash 492266173)
 x509certificate (key crypto-key) (issuer-name string) (not-before string)
 (not-after string))

(defgmethod (crypto+sign :class 'crypto :bind "sign" :hash 1673662703)
 packed-byte-array (hash-type hashing-context+hash-type)
 (hash packed-byte-array) (key crypto-key))

(defgmethod (crypto+verify :class 'crypto :bind "verify" :hash 2805902225) bool
 (hash-type hashing-context+hash-type) (hash packed-byte-array)
 (signature packed-byte-array) (key crypto-key))

(defgmethod (crypto+encrypt :class 'crypto :bind "encrypt" :hash 2361793670)
 packed-byte-array (key crypto-key) (plaintext packed-byte-array))

(defgmethod (crypto+decrypt :class 'crypto :bind "decrypt" :hash 2361793670)
 packed-byte-array (key crypto-key) (ciphertext packed-byte-array))

(defgmethod
 (crypto+hmac-digest :class 'crypto :bind "hmac_digest" :hash 2368951203)
 packed-byte-array (hash-type hashing-context+hash-type)
 (key packed-byte-array) (msg packed-byte-array))

(defgmethod
 (crypto+constant-time-compare :class 'crypto :bind "constant_time_compare"
  :hash 1024142237)
 bool (trusted packed-byte-array) (received packed-byte-array))