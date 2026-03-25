(common-lisp:in-package :%godot)


(defgmethod
 (tlsoptions+client :class 'tlsoptions :bind "client" :hash 3565000357 :static
  common-lisp:t)
 tlsoptions (trusted-chain x509certificate) (common-name-override string))

(defgmethod
 (tlsoptions+client-unsafe :class 'tlsoptions :bind "client_unsafe" :hash
  2090251749 :static common-lisp:t)
 tlsoptions (trusted-chain x509certificate))

(defgmethod
 (tlsoptions+server :class 'tlsoptions :bind "server" :hash 36969539 :static
  common-lisp:t)
 tlsoptions (key crypto-key) (certificate x509certificate))

(defgmethod
 (tlsoptions+is-server :class 'tlsoptions :bind "is_server" :hash 36873697)
 bool)

(defgmethod
 (tlsoptions+is-unsafe-client :class 'tlsoptions :bind "is_unsafe_client" :hash
  36873697)
 bool)

(defgmethod
 (tlsoptions+get-common-name-override :class 'tlsoptions :bind
  "get_common_name_override" :hash 201670096)
 string)

(defgmethod
 (tlsoptions+get-trusted-ca-chain :class 'tlsoptions :bind
  "get_trusted_ca_chain" :hash 1120709175)
 x509certificate)

(defgmethod
 (tlsoptions+get-private-key :class 'tlsoptions :bind "get_private_key" :hash
  2119971811)
 crypto-key)

(defgmethod
 (tlsoptions+get-own-certificate :class 'tlsoptions :bind "get_own_certificate"
  :hash 1120709175)
 x509certificate)