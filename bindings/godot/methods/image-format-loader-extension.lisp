(common-lisp:in-package :%godot)


(defgmethod
 (image-format-loader-extension+-get-recognized-extensions :class
  'image-format-loader-extension :bind "_get_recognized_extensions" :hash
  1139954409 :virtual common-lisp:t)
 packed-string-array)

(defgmethod
 (image-format-loader-extension+-load-image :class
  'image-format-loader-extension :bind "_load_image" :hash 3760540541 :virtual
  common-lisp:t)
 error (image image) (fileaccess file-access)
 (flags image-format-loader+loader-flags) (scale float))

(defgmethod
 (image-format-loader-extension+add-format-loader :class
  'image-format-loader-extension :bind "add_format_loader" :hash 3218959716)
 :void)

(defgmethod
 (image-format-loader-extension+remove-format-loader :class
  'image-format-loader-extension :bind "remove_format_loader" :hash 3218959716)
 :void)