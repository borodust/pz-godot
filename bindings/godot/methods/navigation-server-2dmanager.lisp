(common-lisp:in-package :%godot)


(defgmethod
 (navigation-server-2dmanager+register-server :class
  'navigation-server-2dmanager :bind "register_server" :hash 2137474292)
 :void (name string) (create-callback callable))

(defgmethod
 (navigation-server-2dmanager+set-default-server :class
  'navigation-server-2dmanager :bind "set_default_server" :hash 2956805083)
 :void (name string) (priority int))