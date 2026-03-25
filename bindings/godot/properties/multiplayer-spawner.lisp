(common-lisp:in-package :%godot)


(defgproperty multiplayer-spawner+spawn-path 'multiplayer-spawner :get
 'multiplayer-spawner+get-spawn-path :set 'multiplayer-spawner+set-spawn-path)

(defgproperty multiplayer-spawner+spawn-limit 'multiplayer-spawner :get
 'multiplayer-spawner+get-spawn-limit :set 'multiplayer-spawner+set-spawn-limit)

(defgproperty multiplayer-spawner+spawn-function 'multiplayer-spawner :get
 'multiplayer-spawner+get-spawn-function :set
 'multiplayer-spawner+set-spawn-function)