(common-lisp:in-package :%godot)


(defgmethod
 (open-xrinteraction-profile-metadata+register-profile-rename :class
  'open-xrinteraction-profile-metadata :bind "register_profile_rename" :hash
  3186203200)
 :void (old-name string) (new-name string))

(defgmethod
 (open-xrinteraction-profile-metadata+register-path-rename :class
  'open-xrinteraction-profile-metadata :bind "register_path_rename" :hash
  3186203200)
 :void (old-name string) (new-name string))

(defgmethod
 (open-xrinteraction-profile-metadata+register-top-level-path :class
  'open-xrinteraction-profile-metadata :bind "register_top_level_path" :hash
  254767734)
 :void (display-name string) (openxr-path string)
 (openxr-extension-names string))

(defgmethod
 (open-xrinteraction-profile-metadata+register-interaction-profile :class
  'open-xrinteraction-profile-metadata :bind "register_interaction_profile"
  :hash 254767734)
 :void (display-name string) (openxr-path string)
 (openxr-extension-names string))

(defgmethod
 (open-xrinteraction-profile-metadata+register-io-path :class
  'open-xrinteraction-profile-metadata :bind "register_io_path" :hash
  3443511926)
 :void (interaction-profile string) (display-name string)
 (toplevel-path string) (openxr-path string) (openxr-extension-names string)
 (action-type open-xraction+action-type))