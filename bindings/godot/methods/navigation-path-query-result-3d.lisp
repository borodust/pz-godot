(common-lisp:in-package :%godot)


(defgmethod
 (navigation-path-query-result-3d+set-path :class
  'navigation-path-query-result-3d :bind "set_path" :hash 334873810)
 :void (path packed-vector-3array))

(defgmethod
 (navigation-path-query-result-3d+get-path :class
  'navigation-path-query-result-3d :bind "get_path" :hash 497664490)
 packed-vector-3array)

(defgmethod
 (navigation-path-query-result-3d+set-path-types :class
  'navigation-path-query-result-3d :bind "set_path_types" :hash 3614634198)
 :void (path-types packed-int-32array))

(defgmethod
 (navigation-path-query-result-3d+get-path-types :class
  'navigation-path-query-result-3d :bind "get_path_types" :hash 1930428628)
 packed-int-32array)

(defgmethod
 (navigation-path-query-result-3d+set-path-rids :class
  'navigation-path-query-result-3d :bind "set_path_rids" :hash 381264803)
 :void (path-rids array))

(defgmethod
 (navigation-path-query-result-3d+get-path-rids :class
  'navigation-path-query-result-3d :bind "get_path_rids" :hash 3995934104)
 array)

(defgmethod
 (navigation-path-query-result-3d+set-path-owner-ids :class
  'navigation-path-query-result-3d :bind "set_path_owner_ids" :hash 3709968205)
 :void (path-owner-ids packed-int-64array))

(defgmethod
 (navigation-path-query-result-3d+get-path-owner-ids :class
  'navigation-path-query-result-3d :bind "get_path_owner_ids" :hash 235988956)
 packed-int-64array)

(defgmethod
 (navigation-path-query-result-3d+set-path-length :class
  'navigation-path-query-result-3d :bind "set_path_length" :hash 373806689)
 :void (length float))

(defgmethod
 (navigation-path-query-result-3d+get-path-length :class
  'navigation-path-query-result-3d :bind "get_path_length" :hash 1740695150)
 float)

(defgmethod
 (navigation-path-query-result-3d+reset :class 'navigation-path-query-result-3d
  :bind "reset" :hash 3218959716)
 :void)