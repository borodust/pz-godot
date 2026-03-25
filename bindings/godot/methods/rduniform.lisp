(common-lisp:in-package :%godot)


(defgmethod
 (rduniform+set-uniform-type :class 'rduniform :bind "set_uniform_type" :hash
  1664894931)
 :void (p-member rendering-device+uniform-type))

(defgmethod
 (rduniform+get-uniform-type :class 'rduniform :bind "get_uniform_type" :hash
  475470040)
 rendering-device+uniform-type)

(defgmethod
 (rduniform+set-binding :class 'rduniform :bind "set_binding" :hash 1286410249)
 :void (p-member int))

(defgmethod
 (rduniform+get-binding :class 'rduniform :bind "get_binding" :hash 3905245786)
 int)

(defgmethod
 (rduniform+add-id :class 'rduniform :bind "add_id" :hash 2722037293) :void
 (id rid))

(defgmethod
 (rduniform+clear-ids :class 'rduniform :bind "clear_ids" :hash 3218959716)
 :void)

(defgmethod
 (rduniform+get-ids :class 'rduniform :bind "get_ids" :hash 3995934104) array)