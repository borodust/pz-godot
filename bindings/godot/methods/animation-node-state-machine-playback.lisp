(common-lisp:in-package :%godot)


(defgmethod
 (animation-node-state-machine-playback+travel :class
  'animation-node-state-machine-playback :bind "travel" :hash 3823612587)
 :void (to-node string-name) (reset-on-teleport bool))

(defgmethod
 (animation-node-state-machine-playback+start :class
  'animation-node-state-machine-playback :bind "start" :hash 3823612587)
 :void (node string-name) (reset bool))

(defgmethod
 (animation-node-state-machine-playback+next :class
  'animation-node-state-machine-playback :bind "next" :hash 3218959716)
 :void)

(defgmethod
 (animation-node-state-machine-playback+stop :class
  'animation-node-state-machine-playback :bind "stop" :hash 3218959716)
 :void)

(defgmethod
 (animation-node-state-machine-playback+is-playing :class
  'animation-node-state-machine-playback :bind "is_playing" :hash 36873697)
 bool)

(defgmethod
 (animation-node-state-machine-playback+get-current-node :class
  'animation-node-state-machine-playback :bind "get_current_node" :hash
  2002593661)
 string-name)

(defgmethod
 (animation-node-state-machine-playback+get-current-play-position :class
  'animation-node-state-machine-playback :bind "get_current_play_position"
  :hash 1740695150)
 float)

(defgmethod
 (animation-node-state-machine-playback+get-current-length :class
  'animation-node-state-machine-playback :bind "get_current_length" :hash
  1740695150)
 float)

(defgmethod
 (animation-node-state-machine-playback+get-fading-from-node :class
  'animation-node-state-machine-playback :bind "get_fading_from_node" :hash
  2002593661)
 string-name)

(defgmethod
 (animation-node-state-machine-playback+get-fading-from-play-position :class
  'animation-node-state-machine-playback :bind "get_fading_from_play_position"
  :hash 1740695150)
 float)

(defgmethod
 (animation-node-state-machine-playback+get-fading-from-length :class
  'animation-node-state-machine-playback :bind "get_fading_from_length" :hash
  1740695150)
 float)

(defgmethod
 (animation-node-state-machine-playback+get-fading-position :class
  'animation-node-state-machine-playback :bind "get_fading_position" :hash
  1740695150)
 float)

(defgmethod
 (animation-node-state-machine-playback+get-fading-length :class
  'animation-node-state-machine-playback :bind "get_fading_length" :hash
  1740695150)
 float)

(defgmethod
 (animation-node-state-machine-playback+get-travel-path :class
  'animation-node-state-machine-playback :bind "get_travel_path" :hash
  3995934104)
 array)