(common-lisp:in-package :%godot)


(defgmethod
 (script+can-instantiate :class 'script :bind "can_instantiate" :hash 36873697)
 bool)

(defgmethod
 (script+instance-has :class 'script :bind "instance_has" :hash 397768994) bool
 (base-object object))

(defgmethod
 (script+has-source-code :class 'script :bind "has_source_code" :hash 36873697)
 bool)

(defgmethod
 (script+get-source-code :class 'script :bind "get_source_code" :hash
  201670096)
 string)

(defgmethod
 (script+set-source-code :class 'script :bind "set_source_code" :hash 83702148)
 :void (source string))

(defgmethod (script+reload :class 'script :bind "reload" :hash 1633102583)
 error (keep-state bool))

(defgmethod
 (script+get-base-script :class 'script :bind "get_base_script" :hash
  278624046)
 script)

(defgmethod
 (script+get-instance-base-type :class 'script :bind "get_instance_base_type"
  :hash 2002593661)
 string-name)

(defgmethod
 (script+get-global-name :class 'script :bind "get_global_name" :hash
  2002593661)
 string-name)

(defgmethod
 (script+has-script-signal :class 'script :bind "has_script_signal" :hash
  2619796661)
 bool (signal-name string-name))

(defgmethod
 (script+get-script-property-list :class 'script :bind
  "get_script_property_list" :hash 2915620761)
 array)

(defgmethod
 (script+get-script-method-list :class 'script :bind "get_script_method_list"
  :hash 2915620761)
 array)

(defgmethod
 (script+get-script-signal-list :class 'script :bind "get_script_signal_list"
  :hash 2915620761)
 array)

(defgmethod
 (script+get-script-constant-map :class 'script :bind "get_script_constant_map"
  :hash 2382534195)
 dictionary)

(defgmethod
 (script+get-property-default-value :class 'script :bind
  "get_property_default_value" :hash 2138907829)
 variant (property string-name))

(defgmethod (script+is-tool :class 'script :bind "is_tool" :hash 36873697) bool)

(defgmethod
 (script+is-abstract :class 'script :bind "is_abstract" :hash 36873697) bool)

(defgmethod
 (script+get-rpc-config :class 'script :bind "get_rpc_config" :hash 1214101251)
 variant)