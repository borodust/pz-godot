(common-lisp:in-package :%godot)


(defgmethod
 (scene-replication-config+get-properties :class 'scene-replication-config
  :bind "get_properties" :hash 3995934104)
 array)

(defgmethod
 (scene-replication-config+add-property :class 'scene-replication-config :bind
  "add_property" :hash 4094619021)
 :void (path node-path) (index int))

(defgmethod
 (scene-replication-config+has-property :class 'scene-replication-config :bind
  "has_property" :hash 861721659)
 bool (path node-path))

(defgmethod
 (scene-replication-config+remove-property :class 'scene-replication-config
  :bind "remove_property" :hash 1348162250)
 :void (path node-path))

(defgmethod
 (scene-replication-config+property-get-index :class 'scene-replication-config
  :bind "property_get_index" :hash 1382022557)
 int (path node-path))

(defgmethod
 (scene-replication-config+property-get-spawn :class 'scene-replication-config
  :bind "property_get_spawn" :hash 3456846888)
 bool (path node-path))

(defgmethod
 (scene-replication-config+property-set-spawn :class 'scene-replication-config
  :bind "property_set_spawn" :hash 3868023870)
 :void (path node-path) (enabled bool))

(defgmethod
 (scene-replication-config+property-get-replication-mode :class
  'scene-replication-config :bind "property_get_replication_mode" :hash
  2870606336)
 scene-replication-config+replication-mode (path node-path))

(defgmethod
 (scene-replication-config+property-set-replication-mode :class
  'scene-replication-config :bind "property_set_replication_mode" :hash
  3200083865)
 :void (path node-path) (mode scene-replication-config+replication-mode))

(defgmethod
 (scene-replication-config+property-get-sync :class 'scene-replication-config
  :bind "property_get_sync" :hash 3456846888)
 bool (path node-path))

(defgmethod
 (scene-replication-config+property-set-sync :class 'scene-replication-config
  :bind "property_set_sync" :hash 3868023870)
 :void (path node-path) (enabled bool))

(defgmethod
 (scene-replication-config+property-get-watch :class 'scene-replication-config
  :bind "property_get_watch" :hash 3456846888)
 bool (path node-path))

(defgmethod
 (scene-replication-config+property-set-watch :class 'scene-replication-config
  :bind "property_set_watch" :hash 3868023870)
 :void (path node-path) (enabled bool))