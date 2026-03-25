(common-lisp:in-package :%godot)


(defgmethod
 (web-rtcmultiplayer-peer+create-server :class 'web-rtcmultiplayer-peer :bind
  "create_server" :hash 2865356025)
 error (channels-config array))

(defgmethod
 (web-rtcmultiplayer-peer+create-client :class 'web-rtcmultiplayer-peer :bind
  "create_client" :hash 2641732907)
 error (peer-id int) (channels-config array))

(defgmethod
 (web-rtcmultiplayer-peer+create-mesh :class 'web-rtcmultiplayer-peer :bind
  "create_mesh" :hash 2641732907)
 error (peer-id int) (channels-config array))

(defgmethod
 (web-rtcmultiplayer-peer+add-peer :class 'web-rtcmultiplayer-peer :bind
  "add_peer" :hash 4078953270)
 error (peer web-rtcpeer-connection) (peer-id int) (unreliable-lifetime int))

(defgmethod
 (web-rtcmultiplayer-peer+remove-peer :class 'web-rtcmultiplayer-peer :bind
  "remove_peer" :hash 1286410249)
 :void (peer-id int))

(defgmethod
 (web-rtcmultiplayer-peer+has-peer :class 'web-rtcmultiplayer-peer :bind
  "has_peer" :hash 3067735520)
 bool (peer-id int))

(defgmethod
 (web-rtcmultiplayer-peer+get-peer :class 'web-rtcmultiplayer-peer :bind
  "get_peer" :hash 3554694381)
 dictionary (peer-id int))

(defgmethod
 (web-rtcmultiplayer-peer+get-peers :class 'web-rtcmultiplayer-peer :bind
  "get_peers" :hash 2382534195)
 dictionary)