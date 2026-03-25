(common-lisp:in-package :%godot)


(defgmethod
 (java-object+get-java-class :class 'java-object :bind "get_java_class" :hash
  541536347)
 java-class)

(defgmethod
 (java-object+has-java-method :class 'java-object :bind "has_java_method" :hash
  2619796661)
 bool (method string-name))