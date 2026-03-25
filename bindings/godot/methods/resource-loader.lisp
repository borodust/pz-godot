(common-lisp:in-package :%godot)


(defgmethod
 (resource-loader+load-threaded-request :class 'resource-loader :bind
  "load_threaded_request" :hash 3614384323)
 error (path string) (type-hint string) (use-sub-threads bool)
 (cache-mode resource-loader+cache-mode))

(defgmethod
 (resource-loader+load-threaded-get-status :class 'resource-loader :bind
  "load_threaded_get_status" :hash 4137685479)
 resource-loader+thread-load-status (path string) (progress array))

(defgmethod
 (resource-loader+load-threaded-get :class 'resource-loader :bind
  "load_threaded_get" :hash 1748875256)
 resource (path string))

(defgmethod
 (resource-loader+load :class 'resource-loader :bind "load" :hash 3358495409)
 resource (path string) (type-hint string)
 (cache-mode resource-loader+cache-mode))

(defgmethod
 (resource-loader+get-recognized-extensions-for-type :class 'resource-loader
  :bind "get_recognized_extensions_for_type" :hash 3538744774)
 packed-string-array (type string))

(defgmethod
 (resource-loader+add-resource-format-loader :class 'resource-loader :bind
  "add_resource_format_loader" :hash 2896595483)
 :void (format-loader resource-format-loader) (at-front bool))

(defgmethod
 (resource-loader+remove-resource-format-loader :class 'resource-loader :bind
  "remove_resource_format_loader" :hash 405397102)
 :void (format-loader resource-format-loader))

(defgmethod
 (resource-loader+set-abort-on-missing-resources :class 'resource-loader :bind
  "set_abort_on_missing_resources" :hash 2586408642)
 :void (abort bool))

(defgmethod
 (resource-loader+get-dependencies :class 'resource-loader :bind
  "get_dependencies" :hash 3538744774)
 packed-string-array (path string))

(defgmethod
 (resource-loader+has-cached :class 'resource-loader :bind "has_cached" :hash
  2323990056)
 bool (path string))

(defgmethod
 (resource-loader+get-cached-ref :class 'resource-loader :bind "get_cached_ref"
  :hash 1748875256)
 resource (path string))

(defgmethod
 (resource-loader+exists :class 'resource-loader :bind "exists" :hash
  4185558881)
 bool (path string) (type-hint string))

(defgmethod
 (resource-loader+get-resource-uid :class 'resource-loader :bind
  "get_resource_uid" :hash 1597066294)
 int (path string))

(defgmethod
 (resource-loader+list-directory :class 'resource-loader :bind "list_directory"
  :hash 3538744774)
 packed-string-array (directory-path string))