(common-lisp:in-package :%godot)


(defgmethod
 (multiplayer-synchronizer+set-root-path :class 'multiplayer-synchronizer :bind
  "set_root_path" :hash 1348162250)
 :void (path node-path))

(defgmethod
 (multiplayer-synchronizer+get-root-path :class 'multiplayer-synchronizer :bind
  "get_root_path" :hash 4075236667)
 node-path)

(defgmethod
 (multiplayer-synchronizer+set-replication-interval :class
  'multiplayer-synchronizer :bind "set_replication_interval" :hash 373806689)
 :void (milliseconds float))

(defgmethod
 (multiplayer-synchronizer+get-replication-interval :class
  'multiplayer-synchronizer :bind "get_replication_interval" :hash 1740695150)
 float)

(defgmethod
 (multiplayer-synchronizer+set-delta-interval :class 'multiplayer-synchronizer
  :bind "set_delta_interval" :hash 373806689)
 :void (milliseconds float))

(defgmethod
 (multiplayer-synchronizer+get-delta-interval :class 'multiplayer-synchronizer
  :bind "get_delta_interval" :hash 1740695150)
 float)

(defgmethod
 (multiplayer-synchronizer+set-replication-config :class
  'multiplayer-synchronizer :bind "set_replication_config" :hash 3889206742)
 :void (config scene-replication-config))

(defgmethod
 (multiplayer-synchronizer+get-replication-config :class
  'multiplayer-synchronizer :bind "get_replication_config" :hash 3200254614)
 scene-replication-config)

(defgmethod
 (multiplayer-synchronizer+set-visibility-update-mode :class
  'multiplayer-synchronizer :bind "set_visibility_update_mode" :hash
  3494860300)
 :void (mode multiplayer-synchronizer+visibility-update-mode))

(defgmethod
 (multiplayer-synchronizer+get-visibility-update-mode :class
  'multiplayer-synchronizer :bind "get_visibility_update_mode" :hash
  3352241418)
 multiplayer-synchronizer+visibility-update-mode)

(defgmethod
 (multiplayer-synchronizer+update-visibility :class 'multiplayer-synchronizer
  :bind "update_visibility" :hash 1995695955)
 :void (for-peer int))

(defgmethod
 (multiplayer-synchronizer+set-visibility-public :class
  'multiplayer-synchronizer :bind "set_visibility_public" :hash 2586408642)
 :void (visible bool))

(defgmethod
 (multiplayer-synchronizer+is-visibility-public :class
  'multiplayer-synchronizer :bind "is_visibility_public" :hash 36873697)
 bool)

(defgmethod
 (multiplayer-synchronizer+add-visibility-filter :class
  'multiplayer-synchronizer :bind "add_visibility_filter" :hash 1611583062)
 :void (filter callable))

(defgmethod
 (multiplayer-synchronizer+remove-visibility-filter :class
  'multiplayer-synchronizer :bind "remove_visibility_filter" :hash 1611583062)
 :void (filter callable))

(defgmethod
 (multiplayer-synchronizer+set-visibility-for :class 'multiplayer-synchronizer
  :bind "set_visibility_for" :hash 300928843)
 :void (peer int) (visible bool))

(defgmethod
 (multiplayer-synchronizer+get-visibility-for :class 'multiplayer-synchronizer
  :bind "get_visibility_for" :hash 1116898809)
 bool (peer int))