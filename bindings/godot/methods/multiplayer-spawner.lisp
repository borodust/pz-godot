(common-lisp:in-package :%godot)


(defgmethod
 (multiplayer-spawner+add-spawnable-scene :class 'multiplayer-spawner :bind
  "add_spawnable_scene" :hash 83702148)
 :void (path string))

(defgmethod
 (multiplayer-spawner+get-spawnable-scene-count :class 'multiplayer-spawner
  :bind "get_spawnable_scene_count" :hash 3905245786)
 int)

(defgmethod
 (multiplayer-spawner+get-spawnable-scene :class 'multiplayer-spawner :bind
  "get_spawnable_scene" :hash 844755477)
 string (index int))

(defgmethod
 (multiplayer-spawner+clear-spawnable-scenes :class 'multiplayer-spawner :bind
  "clear_spawnable_scenes" :hash 3218959716)
 :void)

(defgmethod
 (multiplayer-spawner+spawn :class 'multiplayer-spawner :bind "spawn" :hash
  1991184589)
 node (data variant))

(defgmethod
 (multiplayer-spawner+get-spawn-path :class 'multiplayer-spawner :bind
  "get_spawn_path" :hash 4075236667)
 node-path)

(defgmethod
 (multiplayer-spawner+set-spawn-path :class 'multiplayer-spawner :bind
  "set_spawn_path" :hash 1348162250)
 :void (path node-path))

(defgmethod
 (multiplayer-spawner+get-spawn-limit :class 'multiplayer-spawner :bind
  "get_spawn_limit" :hash 3905245786)
 int)

(defgmethod
 (multiplayer-spawner+set-spawn-limit :class 'multiplayer-spawner :bind
  "set_spawn_limit" :hash 1286410249)
 :void (limit int))

(defgmethod
 (multiplayer-spawner+get-spawn-function :class 'multiplayer-spawner :bind
  "get_spawn_function" :hash 1307783378)
 callable)

(defgmethod
 (multiplayer-spawner+set-spawn-function :class 'multiplayer-spawner :bind
  "set_spawn_function" :hash 1611583062)
 :void (spawn-function callable))