(common-lisp:in-package :%godot)


(defgmethod
 (editor-file-system+get-filesystem :class 'editor-file-system :bind
  "get_filesystem" :hash 842323275)
 editor-file-system-directory)

(defgmethod
 (editor-file-system+is-scanning :class 'editor-file-system :bind "is_scanning"
  :hash 36873697)
 bool)

(defgmethod
 (editor-file-system+is-importing :class 'editor-file-system :bind
  "is_importing" :hash 36873697)
 bool)

(defgmethod
 (editor-file-system+get-scanning-progress :class 'editor-file-system :bind
  "get_scanning_progress" :hash 1740695150)
 float)

(defgmethod
 (editor-file-system+scan :class 'editor-file-system :bind "scan" :hash
  3218959716)
 :void)

(defgmethod
 (editor-file-system+scan-sources :class 'editor-file-system :bind
  "scan_sources" :hash 3218959716)
 :void)

(defgmethod
 (editor-file-system+update-file :class 'editor-file-system :bind "update_file"
  :hash 83702148)
 :void (path string))

(defgmethod
 (editor-file-system+get-filesystem-path :class 'editor-file-system :bind
  "get_filesystem_path" :hash 3188521125)
 editor-file-system-directory (path string))

(defgmethod
 (editor-file-system+get-file-type :class 'editor-file-system :bind
  "get_file_type" :hash 3135753539)
 string (path string))

(defgmethod
 (editor-file-system+reimport-files :class 'editor-file-system :bind
  "reimport_files" :hash 4015028928)
 :void (files packed-string-array))