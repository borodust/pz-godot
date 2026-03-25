(common-lisp:in-package :%godot)


(defgproperty scene-tree+auto-accept-quit 'scene-tree :get
 'scene-tree+is-auto-accept-quit :set 'scene-tree+set-auto-accept-quit)

(defgproperty scene-tree+quit-on-go-back 'scene-tree :get
 'scene-tree+is-quit-on-go-back :set 'scene-tree+set-quit-on-go-back)

(defgproperty scene-tree+debug-collisions-hint 'scene-tree :get
 'scene-tree+is-debugging-collisions-hint :set
 'scene-tree+set-debug-collisions-hint)

(defgproperty scene-tree+debug-paths-hint 'scene-tree :get
 'scene-tree+is-debugging-paths-hint :set 'scene-tree+set-debug-paths-hint)

(defgproperty scene-tree+debug-navigation-hint 'scene-tree :get
 'scene-tree+is-debugging-navigation-hint :set
 'scene-tree+set-debug-navigation-hint)

(defgproperty scene-tree+paused 'scene-tree :get 'scene-tree+is-paused :set
 'scene-tree+set-pause)

(defgproperty scene-tree+edited-scene-root 'scene-tree :get
 'scene-tree+get-edited-scene-root :set 'scene-tree+set-edited-scene-root)

(defgproperty scene-tree+current-scene 'scene-tree :get
 'scene-tree+get-current-scene :set 'scene-tree+set-current-scene)

(defgproperty scene-tree+root 'scene-tree :get 'scene-tree+get-root)

(defgproperty scene-tree+multiplayer-poll 'scene-tree :get
 'scene-tree+is-multiplayer-poll-enabled :set
 'scene-tree+set-multiplayer-poll-enabled)

(defgproperty scene-tree+physics-interpolation 'scene-tree :get
 'scene-tree+is-physics-interpolation-enabled :set
 'scene-tree+set-physics-interpolation-enabled)