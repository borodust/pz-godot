(common-lisp:in-package :%godot)


(defgmethod
 (resource-preloader+add-resource :class 'resource-preloader :bind
  "add_resource" :hash 1168801743)
 :void (name string-name) (resource resource))

(defgmethod
 (resource-preloader+remove-resource :class 'resource-preloader :bind
  "remove_resource" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (resource-preloader+rename-resource :class 'resource-preloader :bind
  "rename_resource" :hash 3740211285)
 :void (name string-name) (newname string-name))

(defgmethod
 (resource-preloader+has-resource :class 'resource-preloader :bind
  "has_resource" :hash 2619796661)
 bool (name string-name))

(defgmethod
 (resource-preloader+get-resource :class 'resource-preloader :bind
  "get_resource" :hash 3742749261)
 resource (name string-name))

(defgmethod
 (resource-preloader+get-resource-list :class 'resource-preloader :bind
  "get_resource_list" :hash 1139954409)
 packed-string-array)