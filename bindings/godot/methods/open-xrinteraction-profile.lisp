(common-lisp:in-package :%godot)


(defgmethod
 (open-xrinteraction-profile+set-interaction-profile-path :class
  'open-xrinteraction-profile :bind "set_interaction_profile_path" :hash
  83702148)
 :void (interaction-profile-path string))

(defgmethod
 (open-xrinteraction-profile+get-interaction-profile-path :class
  'open-xrinteraction-profile :bind "get_interaction_profile_path" :hash
  201670096)
 string)

(defgmethod
 (open-xrinteraction-profile+get-binding-count :class
  'open-xrinteraction-profile :bind "get_binding_count" :hash 3905245786)
 int)

(defgmethod
 (open-xrinteraction-profile+get-binding :class 'open-xrinteraction-profile
  :bind "get_binding" :hash 3934429652)
 open-xripbinding (index int))

(defgmethod
 (open-xrinteraction-profile+set-bindings :class 'open-xrinteraction-profile
  :bind "set_bindings" :hash 381264803)
 :void (bindings array))

(defgmethod
 (open-xrinteraction-profile+get-bindings :class 'open-xrinteraction-profile
  :bind "get_bindings" :hash 3995934104)
 array)

(defgmethod
 (open-xrinteraction-profile+get-binding-modifier-count :class
  'open-xrinteraction-profile :bind "get_binding_modifier_count" :hash
  3905245786)
 int)

(defgmethod
 (open-xrinteraction-profile+get-binding-modifier :class
  'open-xrinteraction-profile :bind "get_binding_modifier" :hash 2419896583)
 open-xripbinding-modifier (index int))

(defgmethod
 (open-xrinteraction-profile+set-binding-modifiers :class
  'open-xrinteraction-profile :bind "set_binding_modifiers" :hash 381264803)
 :void (binding-modifiers array))

(defgmethod
 (open-xrinteraction-profile+get-binding-modifiers :class
  'open-xrinteraction-profile :bind "get_binding_modifiers" :hash 3995934104)
 array)