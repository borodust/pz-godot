(common-lisp:in-package :%godot)


(defgmethod
 (editor-script-picker+set-script-owner :class 'editor-script-picker :bind
  "set_script_owner" :hash 1078189570)
 :void (owner-node node))

(defgmethod
 (editor-script-picker+get-script-owner :class 'editor-script-picker :bind
  "get_script_owner" :hash 3160264692)
 node)