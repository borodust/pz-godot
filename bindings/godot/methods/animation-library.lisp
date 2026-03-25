(common-lisp:in-package :%godot)


(defgmethod
 (animation-library+add-animation :class 'animation-library :bind
  "add_animation" :hash 1811855551)
 error (name string-name) (animation animation))

(defgmethod
 (animation-library+remove-animation :class 'animation-library :bind
  "remove_animation" :hash 3304788590)
 :void (name string-name))

(defgmethod
 (animation-library+rename-animation :class 'animation-library :bind
  "rename_animation" :hash 3740211285)
 :void (name string-name) (newname string-name))

(defgmethod
 (animation-library+has-animation :class 'animation-library :bind
  "has_animation" :hash 2619796661)
 bool (name string-name))

(defgmethod
 (animation-library+get-animation :class 'animation-library :bind
  "get_animation" :hash 2933122410)
 animation (name string-name))

(defgmethod
 (animation-library+get-animation-list :class 'animation-library :bind
  "get_animation_list" :hash 3995934104)
 array)

(defgmethod
 (animation-library+get-animation-list-size :class 'animation-library :bind
  "get_animation_list_size" :hash 3905245786)
 int)