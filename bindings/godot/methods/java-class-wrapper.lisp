(common-lisp:in-package :%godot)


(defgmethod
 (java-class-wrapper+wrap :class 'java-class-wrapper :bind "wrap" :hash
  1124367868)
 java-class (name string))

(defgmethod
 (java-class-wrapper+get-exception :class 'java-class-wrapper :bind
  "get_exception" :hash 3277089691)
 java-object)

(defgmethod
 (java-class-wrapper+create-sam-callback :class 'java-class-wrapper :bind
  "create_sam_callback" :hash 2479014754)
 java-object (sam-interface string) (callable callable))

(defgmethod
 (java-class-wrapper+create-proxy :class 'java-class-wrapper :bind
  "create_proxy" :hash 2694931752)
 java-object (object object) (interfaces packed-string-array))