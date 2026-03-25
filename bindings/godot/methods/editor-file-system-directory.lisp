(common-lisp:in-package :%godot)


(defgmethod
 (editor-file-system-directory+get-subdir-count :class
  'editor-file-system-directory :bind "get_subdir_count" :hash 3905245786)
 int)

(defgmethod
 (editor-file-system-directory+get-subdir :class 'editor-file-system-directory
  :bind "get_subdir" :hash 2330964164)
 editor-file-system-directory (idx int))

(defgmethod
 (editor-file-system-directory+get-file-count :class
  'editor-file-system-directory :bind "get_file_count" :hash 3905245786)
 int)

(defgmethod
 (editor-file-system-directory+get-file :class 'editor-file-system-directory
  :bind "get_file" :hash 844755477)
 string (idx int))

(defgmethod
 (editor-file-system-directory+get-file-path :class
  'editor-file-system-directory :bind "get_file_path" :hash 844755477)
 string (idx int))

(defgmethod
 (editor-file-system-directory+get-file-type :class
  'editor-file-system-directory :bind "get_file_type" :hash 659327637)
 string-name (idx int))

(defgmethod
 (editor-file-system-directory+get-file-script-class-name :class
  'editor-file-system-directory :bind "get_file_script_class_name" :hash
  844755477)
 string (idx int))

(defgmethod
 (editor-file-system-directory+get-file-script-class-extends :class
  'editor-file-system-directory :bind "get_file_script_class_extends" :hash
  844755477)
 string (idx int))

(defgmethod
 (editor-file-system-directory+get-file-import-is-valid :class
  'editor-file-system-directory :bind "get_file_import_is_valid" :hash
  1116898809)
 bool (idx int))

(defgmethod
 (editor-file-system-directory+get-name :class 'editor-file-system-directory
  :bind "get_name" :hash 2841200299)
 string)

(defgmethod
 (editor-file-system-directory+get-path :class 'editor-file-system-directory
  :bind "get_path" :hash 201670096)
 string)

(defgmethod
 (editor-file-system-directory+get-parent :class 'editor-file-system-directory
  :bind "get_parent" :hash 842323275)
 editor-file-system-directory)

(defgmethod
 (editor-file-system-directory+find-file-index :class
  'editor-file-system-directory :bind "find_file_index" :hash 1321353865)
 int (name string))

(defgmethod
 (editor-file-system-directory+find-dir-index :class
  'editor-file-system-directory :bind "find_dir_index" :hash 1321353865)
 int (name string))