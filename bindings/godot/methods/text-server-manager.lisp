(common-lisp:in-package :%godot)


(defgmethod
 (text-server-manager+add-interface :class 'text-server-manager :bind
  "add_interface" :hash 1799689403)
 :void (interface text-server))

(defgmethod
 (text-server-manager+get-interface-count :class 'text-server-manager :bind
  "get_interface_count" :hash 3905245786)
 int)

(defgmethod
 (text-server-manager+remove-interface :class 'text-server-manager :bind
  "remove_interface" :hash 1799689403)
 :void (interface text-server))

(defgmethod
 (text-server-manager+get-interface :class 'text-server-manager :bind
  "get_interface" :hash 1672475555)
 text-server (idx int))

(defgmethod
 (text-server-manager+get-interfaces :class 'text-server-manager :bind
  "get_interfaces" :hash 3995934104)
 array)

(defgmethod
 (text-server-manager+find-interface :class 'text-server-manager :bind
  "find_interface" :hash 2240905781)
 text-server (name string))

(defgmethod
 (text-server-manager+set-primary-interface :class 'text-server-manager :bind
  "set_primary_interface" :hash 1799689403)
 :void (index text-server))

(defgmethod
 (text-server-manager+get-primary-interface :class 'text-server-manager :bind
  "get_primary_interface" :hash 905850878)
 text-server)