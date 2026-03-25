(common-lisp:in-package :%godot)


(defgmethod
 (missing-node+set-original-class :class 'missing-node :bind
  "set_original_class" :hash 83702148)
 :void (name string))

(defgmethod
 (missing-node+get-original-class :class 'missing-node :bind
  "get_original_class" :hash 201670096)
 string)

(defgmethod
 (missing-node+set-original-scene :class 'missing-node :bind
  "set_original_scene" :hash 83702148)
 :void (name string))

(defgmethod
 (missing-node+get-original-scene :class 'missing-node :bind
  "get_original_scene" :hash 201670096)
 string)

(defgmethod
 (missing-node+set-recording-properties :class 'missing-node :bind
  "set_recording_properties" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (missing-node+is-recording-properties :class 'missing-node :bind
  "is_recording_properties" :hash 36873697)
 bool)

(defgmethod
 (missing-node+set-recording-signals :class 'missing-node :bind
  "set_recording_signals" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (missing-node+is-recording-signals :class 'missing-node :bind
  "is_recording_signals" :hash 36873697)
 bool)