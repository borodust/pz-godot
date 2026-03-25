(common-lisp:in-package :%godot)


(defgmethod
 (editor-export-platform+get-os-name :class 'editor-export-platform :bind
  "get_os_name" :hash 201670096)
 string)

(defgmethod
 (editor-export-platform+create-preset :class 'editor-export-platform :bind
  "create_preset" :hash 2572397818)
 editor-export-preset)

(defgmethod
 (editor-export-platform+find-export-template :class 'editor-export-platform
  :bind "find_export_template" :hash 2248993622)
 dictionary (template-file-name string))

(defgmethod
 (editor-export-platform+get-current-presets :class 'editor-export-platform
  :bind "get_current_presets" :hash 3995934104)
 array)

(defgmethod
 (editor-export-platform+save-pack :class 'editor-export-platform :bind
  "save_pack" :hash 3420080977)
 dictionary (preset editor-export-preset) (debug bool) (path string)
 (embed bool))

(defgmethod
 (editor-export-platform+save-zip :class 'editor-export-platform :bind
  "save_zip" :hash 1485052307)
 dictionary (preset editor-export-preset) (debug bool) (path string))

(defgmethod
 (editor-export-platform+save-pack-patch :class 'editor-export-platform :bind
  "save_pack_patch" :hash 1485052307)
 dictionary (preset editor-export-preset) (debug bool) (path string))

(defgmethod
 (editor-export-platform+save-zip-patch :class 'editor-export-platform :bind
  "save_zip_patch" :hash 1485052307)
 dictionary (preset editor-export-preset) (debug bool) (path string))

(defgmethod
 (editor-export-platform+gen-export-flags :class 'editor-export-platform :bind
  "gen_export_flags" :hash 2976483270)
 packed-string-array (flags editor-export-platform+debug-flags))

(defgmethod
 (editor-export-platform+export-project-files :class 'editor-export-platform
  :bind "export_project_files" :hash 1063735070)
 error (preset editor-export-preset) (debug bool) (save-cb callable)
 (shared-cb callable))

(defgmethod
 (editor-export-platform+export-project :class 'editor-export-platform :bind
  "export_project" :hash 3879521245)
 error (preset editor-export-preset) (debug bool) (path string)
 (flags editor-export-platform+debug-flags))

(defgmethod
 (editor-export-platform+export-pack :class 'editor-export-platform :bind
  "export_pack" :hash 3879521245)
 error (preset editor-export-preset) (debug bool) (path string)
 (flags editor-export-platform+debug-flags))

(defgmethod
 (editor-export-platform+export-zip :class 'editor-export-platform :bind
  "export_zip" :hash 3879521245)
 error (preset editor-export-preset) (debug bool) (path string)
 (flags editor-export-platform+debug-flags))

(defgmethod
 (editor-export-platform+export-pack-patch :class 'editor-export-platform :bind
  "export_pack_patch" :hash 608021658)
 error (preset editor-export-preset) (debug bool) (path string)
 (patches packed-string-array) (flags editor-export-platform+debug-flags))

(defgmethod
 (editor-export-platform+export-zip-patch :class 'editor-export-platform :bind
  "export_zip_patch" :hash 608021658)
 error (preset editor-export-preset) (debug bool) (path string)
 (patches packed-string-array) (flags editor-export-platform+debug-flags))

(defgmethod
 (editor-export-platform+clear-messages :class 'editor-export-platform :bind
  "clear_messages" :hash 3218959716)
 :void)

(defgmethod
 (editor-export-platform+add-message :class 'editor-export-platform :bind
  "add_message" :hash 782767225)
 :void (type editor-export-platform+export-message-type) (category string)
 (message string))

(defgmethod
 (editor-export-platform+get-message-count :class 'editor-export-platform :bind
  "get_message_count" :hash 3905245786)
 int)

(defgmethod
 (editor-export-platform+get-message-type :class 'editor-export-platform :bind
  "get_message_type" :hash 2667287293)
 editor-export-platform+export-message-type (index int))

(defgmethod
 (editor-export-platform+get-message-category :class 'editor-export-platform
  :bind "get_message_category" :hash 844755477)
 string (index int))

(defgmethod
 (editor-export-platform+get-message-text :class 'editor-export-platform :bind
  "get_message_text" :hash 844755477)
 string (index int))

(defgmethod
 (editor-export-platform+get-worst-message-type :class 'editor-export-platform
  :bind "get_worst_message_type" :hash 2580557466)
 editor-export-platform+export-message-type)

(defgmethod
 (editor-export-platform+ssh-run-on-remote :class 'editor-export-platform :bind
  "ssh_run_on_remote" :hash 3163734797)
 error (host string) (port string) (ssh-arg packed-string-array)
 (cmd-args string) (output array) (port-fwd int))

(defgmethod
 (editor-export-platform+ssh-run-on-remote-no-wait :class
  'editor-export-platform :bind "ssh_run_on_remote_no_wait" :hash 3606362233)
 int (host string) (port string) (ssh-args packed-string-array)
 (cmd-args string) (port-fwd int))

(defgmethod
 (editor-export-platform+ssh-push-to-remote :class 'editor-export-platform
  :bind "ssh_push_to_remote" :hash 218756989)
 error (host string) (port string) (scp-args packed-string-array)
 (src-file string) (dst-file string))

(defgmethod
 (editor-export-platform+get-internal-export-files :class
  'editor-export-platform :bind "get_internal_export_files" :hash 89550086)
 dictionary (preset editor-export-preset) (debug bool))

(defgmethod
 (editor-export-platform+get-forced-export-files :class 'editor-export-platform
  :bind "get_forced_export_files" :hash 1939331020 :static common-lisp:t)
 packed-string-array (preset editor-export-preset))