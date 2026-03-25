(common-lisp:in-package :%godot)


(defgmethod
 (dir-access+open :class 'dir-access :bind "open" :hash 1923528528 :static
  common-lisp:t)
 dir-access (path string))

(defgmethod
 (dir-access+get-open-error :class 'dir-access :bind "get_open_error" :hash
  166280745 :static common-lisp:t)
 error)

(defgmethod
 (dir-access+create-temp :class 'dir-access :bind "create_temp" :hash 812913566
  :static common-lisp:t)
 dir-access (prefix string) (keep bool))

(defgmethod
 (dir-access+list-dir-begin :class 'dir-access :bind "list_dir_begin" :hash
  166280745)
 error)

(defgmethod
 (dir-access+get-next :class 'dir-access :bind "get_next" :hash 2841200299)
 string)

(defgmethod
 (dir-access+current-is-dir :class 'dir-access :bind "current_is_dir" :hash
  36873697)
 bool)

(defgmethod
 (dir-access+list-dir-end :class 'dir-access :bind "list_dir_end" :hash
  3218959716)
 :void)

(defgmethod
 (dir-access+get-files :class 'dir-access :bind "get_files" :hash 2981934095)
 packed-string-array)

(defgmethod
 (dir-access+get-files-at :class 'dir-access :bind "get_files_at" :hash
  3538744774 :static common-lisp:t)
 packed-string-array (path string))

(defgmethod
 (dir-access+get-directories :class 'dir-access :bind "get_directories" :hash
  2981934095)
 packed-string-array)

(defgmethod
 (dir-access+get-directories-at :class 'dir-access :bind "get_directories_at"
  :hash 3538744774 :static common-lisp:t)
 packed-string-array (path string))

(defgmethod
 (dir-access+get-drive-count :class 'dir-access :bind "get_drive_count" :hash
  2455072627 :static common-lisp:t)
 int)

(defgmethod
 (dir-access+get-drive-name :class 'dir-access :bind "get_drive_name" :hash
  990163283 :static common-lisp:t)
 string (idx int))

(defgmethod
 (dir-access+get-current-drive :class 'dir-access :bind "get_current_drive"
  :hash 2455072627)
 int)

(defgmethod
 (dir-access+change-dir :class 'dir-access :bind "change_dir" :hash 166001499)
 error (to-dir string))

(defgmethod
 (dir-access+get-current-dir :class 'dir-access :bind "get_current_dir" :hash
  1287308131)
 string (include-drive bool))

(defgmethod
 (dir-access+make-dir :class 'dir-access :bind "make_dir" :hash 166001499)
 error (path string))

(defgmethod
 (dir-access+make-dir-absolute :class 'dir-access :bind "make_dir_absolute"
  :hash 166001499 :static common-lisp:t)
 error (path string))

(defgmethod
 (dir-access+make-dir-recursive :class 'dir-access :bind "make_dir_recursive"
  :hash 166001499)
 error (path string))

(defgmethod
 (dir-access+make-dir-recursive-absolute :class 'dir-access :bind
  "make_dir_recursive_absolute" :hash 166001499 :static common-lisp:t)
 error (path string))

(defgmethod
 (dir-access+file-exists :class 'dir-access :bind "file_exists" :hash
  2323990056)
 bool (path string))

(defgmethod
 (dir-access+dir-exists :class 'dir-access :bind "dir_exists" :hash 2323990056)
 bool (path string))

(defgmethod
 (dir-access+dir-exists-absolute :class 'dir-access :bind "dir_exists_absolute"
  :hash 2323990056 :static common-lisp:t)
 bool (path string))

(defgmethod
 (dir-access+get-space-left :class 'dir-access :bind "get_space_left" :hash
  2455072627)
 int)

(defgmethod (dir-access+copy :class 'dir-access :bind "copy" :hash 1063198817)
 error (from string) (to string) (chmod-flags int))

(defgmethod
 (dir-access+copy-absolute :class 'dir-access :bind "copy_absolute" :hash
  1063198817 :static common-lisp:t)
 error (from string) (to string) (chmod-flags int))

(defgmethod
 (dir-access+rename :class 'dir-access :bind "rename" :hash 852856452) error
 (from string) (to string))

(defgmethod
 (dir-access+rename-absolute :class 'dir-access :bind "rename_absolute" :hash
  852856452 :static common-lisp:t)
 error (from string) (to string))

(defgmethod
 (dir-access+remove :class 'dir-access :bind "remove" :hash 166001499) error
 (path string))

(defgmethod
 (dir-access+remove-absolute :class 'dir-access :bind "remove_absolute" :hash
  166001499 :static common-lisp:t)
 error (path string))

(defgmethod
 (dir-access+is-link :class 'dir-access :bind "is_link" :hash 2323990056) bool
 (path string))

(defgmethod
 (dir-access+read-link :class 'dir-access :bind "read_link" :hash 1703090593)
 string (path string))

(defgmethod
 (dir-access+create-link :class 'dir-access :bind "create_link" :hash
  852856452)
 error (source string) (target string))

(defgmethod
 (dir-access+is-bundle :class 'dir-access :bind "is_bundle" :hash 3927539163)
 bool (path string))

(defgmethod
 (dir-access+set-include-navigational :class 'dir-access :bind
  "set_include_navigational" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (dir-access+get-include-navigational :class 'dir-access :bind
  "get_include_navigational" :hash 36873697)
 bool)

(defgmethod
 (dir-access+set-include-hidden :class 'dir-access :bind "set_include_hidden"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (dir-access+get-include-hidden :class 'dir-access :bind "get_include_hidden"
  :hash 36873697)
 bool)

(defgmethod
 (dir-access+get-filesystem-type :class 'dir-access :bind "get_filesystem_type"
  :hash 201670096)
 string)

(defgmethod
 (dir-access+is-case-sensitive :class 'dir-access :bind "is_case_sensitive"
  :hash 3927539163)
 bool (path string))

(defgmethod
 (dir-access+is-equivalent :class 'dir-access :bind "is_equivalent" :hash
  820780508)
 bool (path-a string) (path-b string))