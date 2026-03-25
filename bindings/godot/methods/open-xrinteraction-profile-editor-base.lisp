(common-lisp:in-package :%godot)


(defgmethod
 (open-xrinteraction-profile-editor-base+setup :class
  'open-xrinteraction-profile-editor-base :bind "setup" :hash 421962938)
 :void (action-map open-xraction-map)
 (interaction-profile open-xrinteraction-profile))