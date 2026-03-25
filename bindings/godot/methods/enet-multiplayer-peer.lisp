(common-lisp:in-package :%godot)


(defgmethod
 (enet-multiplayer-peer+create-server :class 'enet-multiplayer-peer :bind
  "create_server" :hash 2917761309)
 error (port int) (max-clients int) (max-channels int) (in-bandwidth int)
 (out-bandwidth int))

(defgmethod
 (enet-multiplayer-peer+create-client :class 'enet-multiplayer-peer :bind
  "create_client" :hash 2327163476)
 error (address string) (port int) (channel-count int) (in-bandwidth int)
 (out-bandwidth int) (local-port int))

(defgmethod
 (enet-multiplayer-peer+create-mesh :class 'enet-multiplayer-peer :bind
  "create_mesh" :hash 844576869)
 error (unique-id int))

(defgmethod
 (enet-multiplayer-peer+add-mesh-peer :class 'enet-multiplayer-peer :bind
  "add_mesh_peer" :hash 1293458335)
 error (peer-id int) (host enet-connection))

(defgmethod
 (enet-multiplayer-peer+set-bind-ip :class 'enet-multiplayer-peer :bind
  "set_bind_ip" :hash 83702148)
 :void (ip string))

(defgmethod
 (enet-multiplayer-peer+get-host :class 'enet-multiplayer-peer :bind "get_host"
  :hash 4103238886)
 enet-connection)

(defgmethod
 (enet-multiplayer-peer+get-peer :class 'enet-multiplayer-peer :bind "get_peer"
  :hash 3793311544)
 enet-packet-peer (id int))