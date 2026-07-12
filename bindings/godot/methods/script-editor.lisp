(common-lisp:in-package :%godot)


(defgmethod
 (script-editor+get-current-editor :class 'script-editor :bind
  "get_current_editor" :hash 1906266726)
 script-editor-base)

(defgmethod
 (script-editor+get-open-script-editors :class 'script-editor :bind
  "get_open_script_editors" :hash 3995934104)
 array)

(defgmethod
 (script-editor+get-breakpoints :class 'script-editor :bind "get_breakpoints"
  :hash 2981934095)
 packed-string-array)

(defgmethod
 (script-editor+register-syntax-highlighter :class 'script-editor :bind
  "register_syntax_highlighter" :hash 1092774468)
 :void (syntax-highlighter editor-syntax-highlighter))

(defgmethod
 (script-editor+unregister-syntax-highlighter :class 'script-editor :bind
  "unregister_syntax_highlighter" :hash 1092774468)
 :void (syntax-highlighter editor-syntax-highlighter))

(defgmethod
 (script-editor+goto-line :class 'script-editor :bind "goto_line" :hash
  1286410249)
 :void (line-number int))

(defgmethod
 (script-editor+get-current-script :class 'script-editor :bind
  "get_current_script" :hash 2146468882)
 script)

(defgmethod
 (script-editor+get-open-scripts :class 'script-editor :bind "get_open_scripts"
  :hash 3995934104)
 array)

(defgmethod
 (script-editor+open-script-create-dialog :class 'script-editor :bind
  "open_script_create_dialog" :hash 3186203200)
 :void (base-name string) (base-path string))

(defgmethod
 (script-editor+reload-open-files :class 'script-editor :bind
  "reload_open_files" :hash 3218959716)
 :void)

(defgmethod
 (script-editor+goto-help :class 'script-editor :bind "goto_help" :hash
  83702148)
 :void (topic string))

(defgmethod
 (script-editor+update-docs-from-script :class 'script-editor :bind
  "update_docs_from_script" :hash 3657522847)
 :void (script script))

(defgmethod
 (script-editor+clear-docs-from-script :class 'script-editor :bind
  "clear_docs_from_script" :hash 3657522847)
 :void (script script))

(defgmethod
 (script-editor+get-unsaved-files :class 'script-editor :bind
  "get_unsaved_files" :hash 1139954409)
 packed-string-array)

(defgmethod
 (script-editor+save-all-scripts :class 'script-editor :bind "save_all_scripts"
  :hash 3218959716)
 :void)

(defgmethod
 (script-editor+close-file :class 'script-editor :bind "close_file" :hash
  166001499)
 error (path string))