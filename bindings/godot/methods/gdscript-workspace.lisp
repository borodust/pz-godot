(common-lisp:in-package :%godot)


(defgmethod
 (gdscript-workspace+apply-new-signal :class 'gdscript-workspace :bind
  "apply_new_signal" :hash 3682583557)
 :void (obj object) (function string) (args packed-string-array))

(defgmethod
 (gdscript-workspace+get-file-path :class 'gdscript-workspace :bind
  "get_file_path" :hash 1703090593)
 string (uri string))

(defgmethod
 (gdscript-workspace+get-file-uri :class 'gdscript-workspace :bind
  "get_file_uri" :hash 3135753539)
 string (path string))

(defgmethod
 (gdscript-workspace+generate-script-api :class 'gdscript-workspace :bind
  "generate_script_api" :hash 2786125124)
 dictionary (path string))

(defgmethod
 (gdscript-workspace+diddeletefiles :class 'gdscript-workspace :bind
  "didDeleteFiles" :hash 4155329257)
 :void (params dictionary))

(defgmethod
 (gdscript-workspace+parse-script :class 'gdscript-workspace :bind
  "parse_script" :hash 852856452)
 error (path string) (content string))

(defgmethod
 (gdscript-workspace+parse-local-script :class 'gdscript-workspace :bind
  "parse_local_script" :hash 166001499)
 error (path string))

(defgmethod
 (gdscript-workspace+publish-diagnostics :class 'gdscript-workspace :bind
  "publish_diagnostics" :hash 83702148)
 :void (path string))