(common-lisp:in-package :%godot)


(defgmethod
 (open-xrrender-model-manager+get-tracker :class 'open-xrrender-model-manager
  :bind "get_tracker" :hash 2456466356)
 open-xrrender-model-manager+render-model-tracker)

(defgmethod
 (open-xrrender-model-manager+set-tracker :class 'open-xrrender-model-manager
  :bind "set_tracker" :hash 2814627380)
 :void (tracker open-xrrender-model-manager+render-model-tracker))

(defgmethod
 (open-xrrender-model-manager+get-make-local-to-pose :class
  'open-xrrender-model-manager :bind "get_make_local_to_pose" :hash 201670096)
 string)

(defgmethod
 (open-xrrender-model-manager+set-make-local-to-pose :class
  'open-xrrender-model-manager :bind "set_make_local_to_pose" :hash 83702148)
 :void (make-local-to-pose string))