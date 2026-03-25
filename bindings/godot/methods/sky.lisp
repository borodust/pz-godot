(common-lisp:in-package :%godot)


(defgmethod
 (sky+set-radiance-size :class 'sky :bind "set_radiance_size" :hash 1512957179)
 :void (size sky+radiance-size))

(defgmethod
 (sky+get-radiance-size :class 'sky :bind "get_radiance_size" :hash 2708733976)
 sky+radiance-size)

(defgmethod
 (sky+set-process-mode :class 'sky :bind "set_process_mode" :hash 875986769)
 :void (mode sky+process-mode))

(defgmethod
 (sky+get-process-mode :class 'sky :bind "get_process_mode" :hash 731245043)
 sky+process-mode)

(defgmethod
 (sky+set-material :class 'sky :bind "set_material" :hash 2757459619) :void
 (material material))

(defgmethod (sky+get-material :class 'sky :bind "get_material" :hash 5934680)
 material)