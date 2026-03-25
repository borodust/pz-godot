(common-lisp:in-package :%godot)


(defgmethod
 (java-class+get-java-class-name :class 'java-class :bind "get_java_class_name"
  :hash 201670096)
 string)

(defgmethod
 (java-class+get-java-method-list :class 'java-class :bind
  "get_java_method_list" :hash 3995934104)
 array)

(defgmethod
 (java-class+get-java-parent-class :class 'java-class :bind
  "get_java_parent_class" :hash 541536347)
 java-class)

(defgmethod
 (java-class+has-java-method :class 'java-class :bind "has_java_method" :hash
  2619796661)
 bool (method string-name))