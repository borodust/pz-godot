(common-lisp:in-package :%godot)


(defgproperty multiplayer-synchronizer+root-path 'multiplayer-synchronizer :get
 'multiplayer-synchronizer+get-root-path :set
 'multiplayer-synchronizer+set-root-path)

(defgproperty multiplayer-synchronizer+replication-interval
 'multiplayer-synchronizer :get
 'multiplayer-synchronizer+get-replication-interval :set
 'multiplayer-synchronizer+set-replication-interval)

(defgproperty multiplayer-synchronizer+delta-interval 'multiplayer-synchronizer
 :get 'multiplayer-synchronizer+get-delta-interval :set
 'multiplayer-synchronizer+set-delta-interval)

(defgproperty multiplayer-synchronizer+replication-config
 'multiplayer-synchronizer :get
 'multiplayer-synchronizer+get-replication-config :set
 'multiplayer-synchronizer+set-replication-config)

(defgproperty multiplayer-synchronizer+visibility-update-mode
 'multiplayer-synchronizer :get
 'multiplayer-synchronizer+get-visibility-update-mode :set
 'multiplayer-synchronizer+set-visibility-update-mode)

(defgproperty multiplayer-synchronizer+public-visibility
 'multiplayer-synchronizer :get 'multiplayer-synchronizer+is-visibility-public
 :set 'multiplayer-synchronizer+set-visibility-public)