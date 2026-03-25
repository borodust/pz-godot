(common-lisp:in-package :%godot)


(defgproperty open-xrrender-model-manager+tracker 'open-xrrender-model-manager
 :get 'open-xrrender-model-manager+get-tracker :set
 'open-xrrender-model-manager+set-tracker)

(defgproperty open-xrrender-model-manager+make-local-to-pose
 'open-xrrender-model-manager :get
 'open-xrrender-model-manager+get-make-local-to-pose :set
 'open-xrrender-model-manager+set-make-local-to-pose)