(common-lisp:in-package :%godot)


(defgmethod
 (open-xranchor-tracker+has-uuid :class 'open-xranchor-tracker :bind "has_uuid"
  :hash 36873697)
 bool)

(defgmethod
 (open-xranchor-tracker+set-uuid :class 'open-xranchor-tracker :bind "set_uuid"
  :hash 83702148)
 :void (uuid string))

(defgmethod
 (open-xranchor-tracker+get-uuid :class 'open-xranchor-tracker :bind "get_uuid"
  :hash 201670096)
 string)