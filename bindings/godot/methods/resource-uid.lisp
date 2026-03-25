(common-lisp:in-package :%godot)


(defgmethod
 (resource-uid+id-to-text :class 'resource-uid :bind "id_to_text" :hash
  844755477)
 string (id int))

(defgmethod
 (resource-uid+text-to-id :class 'resource-uid :bind "text_to_id" :hash
  1321353865)
 int (text-id string))

(defgmethod
 (resource-uid+create-id :class 'resource-uid :bind "create_id" :hash
  2455072627)
 int)

(defgmethod
 (resource-uid+create-id-for-path :class 'resource-uid :bind
  "create_id_for_path" :hash 1597066294)
 int (path string))

(defgmethod
 (resource-uid+has-id :class 'resource-uid :bind "has_id" :hash 1116898809)
 bool (id int))

(defgmethod
 (resource-uid+add-id :class 'resource-uid :bind "add_id" :hash 501894301)
 :void (id int) (path string))

(defgmethod
 (resource-uid+set-id :class 'resource-uid :bind "set_id" :hash 501894301)
 :void (id int) (path string))

(defgmethod
 (resource-uid+get-id-path :class 'resource-uid :bind "get_id_path" :hash
  844755477)
 string (id int))

(defgmethod
 (resource-uid+remove-id :class 'resource-uid :bind "remove_id" :hash
  1286410249)
 :void (id int))

(defgmethod
 (resource-uid+uid-to-path :class 'resource-uid :bind "uid_to_path" :hash
  1703090593 :static common-lisp:t)
 string (uid string))

(defgmethod
 (resource-uid+path-to-uid :class 'resource-uid :bind "path_to_uid" :hash
  1703090593 :static common-lisp:t)
 string (path string))

(defgmethod
 (resource-uid+ensure-path :class 'resource-uid :bind "ensure_path" :hash
  1703090593 :static common-lisp:t)
 string (path-or-uid string))