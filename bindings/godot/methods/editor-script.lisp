(common-lisp:in-package :%godot)


(defgmethod
 (editor-script+%run :class 'editor-script :bind "_run" :hash 3218959716
  :virtual common-lisp:t)
 :void)

(defgmethod
 (editor-script+add-root-node :class 'editor-script :bind "add_root_node" :hash
  1078189570)
 :void (node node))

(defgmethod
 (editor-script+get-scene :class 'editor-script :bind "get_scene" :hash
  3160264692)
 node)

(defgmethod
 (editor-script+get-editor-interface :class 'editor-script :bind
  "get_editor_interface" :hash 1976662476)
 editor-interface)