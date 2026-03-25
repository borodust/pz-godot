(common-lisp:in-package :%godot)


(defgmethod
 (xrtracker+get-tracker-type :class 'xrtracker :bind "get_tracker_type" :hash
  2784508102)
 xrserver+tracker-type)

(defgmethod
 (xrtracker+set-tracker-type :class 'xrtracker :bind "set_tracker_type" :hash
  3055763575)
 :void (type xrserver+tracker-type))

(defgmethod
 (xrtracker+get-tracker-name :class 'xrtracker :bind "get_tracker_name" :hash
  2002593661)
 string-name)

(defgmethod
 (xrtracker+set-tracker-name :class 'xrtracker :bind "set_tracker_name" :hash
  3304788590)
 :void (name string-name))

(defgmethod
 (xrtracker+get-tracker-desc :class 'xrtracker :bind "get_tracker_desc" :hash
  201670096)
 string)

(defgmethod
 (xrtracker+set-tracker-desc :class 'xrtracker :bind "set_tracker_desc" :hash
  83702148)
 :void (description string))