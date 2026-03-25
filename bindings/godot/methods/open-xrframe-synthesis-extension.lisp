(common-lisp:in-package :%godot)


(defgmethod
 (open-xrframe-synthesis-extension+is-available :class
  'open-xrframe-synthesis-extension :bind "is_available" :hash 36873697)
 bool)

(defgmethod
 (open-xrframe-synthesis-extension+is-enabled :class
  'open-xrframe-synthesis-extension :bind "is_enabled" :hash 36873697)
 bool)

(defgmethod
 (open-xrframe-synthesis-extension+set-enabled :class
  'open-xrframe-synthesis-extension :bind "set_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (open-xrframe-synthesis-extension+get-relax-frame-interval :class
  'open-xrframe-synthesis-extension :bind "get_relax_frame_interval" :hash
  36873697)
 bool)

(defgmethod
 (open-xrframe-synthesis-extension+set-relax-frame-interval :class
  'open-xrframe-synthesis-extension :bind "set_relax_frame_interval" :hash
  2586408642)
 :void (relax-frame-interval bool))

(defgmethod
 (open-xrframe-synthesis-extension+skip-next-frame :class
  'open-xrframe-synthesis-extension :bind "skip_next_frame" :hash 3218959716)
 :void)