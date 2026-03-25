(common-lisp:in-package :%godot)


(defgmethod
 (java-script-bridge+eval :class 'java-script-bridge :bind "eval" :hash
  218087648)
 variant (code string) (use-global-execution-context bool))

(defgmethod
 (java-script-bridge+get-interface :class 'java-script-bridge :bind
  "get_interface" :hash 1355533281)
 java-script-object (interface string))

(defgmethod
 (java-script-bridge+create-callback :class 'java-script-bridge :bind
  "create_callback" :hash 422818440)
 java-script-object (callable callable))

(defgmethod
 (java-script-bridge+is-js-buffer :class 'java-script-bridge :bind
  "is_js_buffer" :hash 821968997)
 bool (javascript-object java-script-object))

(defgmethod
 (java-script-bridge+js-buffer-to-packed-byte-array :class 'java-script-bridge
  :bind "js_buffer_to_packed_byte_array" :hash 64409880)
 packed-byte-array (javascript-buffer java-script-object))

(defgmethod
 (java-script-bridge+create-object :class 'java-script-bridge :bind
  "create_object" :hash 3093893586 :vararg common-lisp:t)
 variant (object string))

(defgmethod
 (java-script-bridge+download-buffer :class 'java-script-bridge :bind
  "download_buffer" :hash 3352272093)
 :void (buffer packed-byte-array) (name string) (mime string))

(defgmethod
 (java-script-bridge+pwa-needs-update :class 'java-script-bridge :bind
  "pwa_needs_update" :hash 36873697)
 bool)

(defgmethod
 (java-script-bridge+pwa-update :class 'java-script-bridge :bind "pwa_update"
  :hash 166280745)
 error)

(defgmethod
 (java-script-bridge+force-fs-sync :class 'java-script-bridge :bind
  "force_fs_sync" :hash 3218959716)
 :void)