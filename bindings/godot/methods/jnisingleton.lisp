(common-lisp:in-package :%godot)


(defgmethod
 (jnisingleton+has-java-method :class 'jnisingleton :bind "has_java_method"
  :hash 2619796661)
 bool (method string-name))