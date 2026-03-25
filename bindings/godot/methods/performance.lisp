(common-lisp:in-package :%godot)


(defgmethod
 (performance+get-monitor :class 'performance :bind "get_monitor" :hash
  1943275655)
 float (monitor performance+monitor))

(defgmethod
 (performance+add-custom-monitor :class 'performance :bind "add_custom_monitor"
  :hash 3655788610)
 :void (id string-name) (callable callable) (arguments array)
 (type performance+monitor-type))

(defgmethod
 (performance+remove-custom-monitor :class 'performance :bind
  "remove_custom_monitor" :hash 3304788590)
 :void (id string-name))

(defgmethod
 (performance+has-custom-monitor :class 'performance :bind "has_custom_monitor"
  :hash 2041966384)
 bool (id string-name))

(defgmethod
 (performance+get-custom-monitor :class 'performance :bind "get_custom_monitor"
  :hash 2138907829)
 variant (id string-name))

(defgmethod
 (performance+get-monitor-modification-time :class 'performance :bind
  "get_monitor_modification_time" :hash 2455072627)
 int)

(defgmethod
 (performance+get-custom-monitor-names :class 'performance :bind
  "get_custom_monitor_names" :hash 2915620761)
 array)

(defgmethod
 (performance+get-custom-monitor-types :class 'performance :bind
  "get_custom_monitor_types" :hash 969006518)
 packed-int-32array)