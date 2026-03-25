(common-lisp:in-package :%godot)


(defgmethod
 (missing-resource+set-original-class :class 'missing-resource :bind
  "set_original_class" :hash 83702148)
 :void (name string))

(defgmethod
 (missing-resource+get-original-class :class 'missing-resource :bind
  "get_original_class" :hash 201670096)
 string)

(defgmethod
 (missing-resource+set-recording-properties :class 'missing-resource :bind
  "set_recording_properties" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (missing-resource+is-recording-properties :class 'missing-resource :bind
  "is_recording_properties" :hash 36873697)
 bool)