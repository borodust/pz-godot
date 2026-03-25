(common-lisp:in-package :%godot)


(defgmethod (xmlparser+read :class 'xmlparser :bind "read" :hash 166280745)
 error)

(defgmethod
 (xmlparser+get-node-type :class 'xmlparser :bind "get_node_type" :hash
  2984359541)
 xmlparser+node-type)

(defgmethod
 (xmlparser+get-node-name :class 'xmlparser :bind "get_node_name" :hash
  201670096)
 string)

(defgmethod
 (xmlparser+get-node-data :class 'xmlparser :bind "get_node_data" :hash
  201670096)
 string)

(defgmethod
 (xmlparser+get-node-offset :class 'xmlparser :bind "get_node_offset" :hash
  3905245786)
 int)

(defgmethod
 (xmlparser+get-attribute-count :class 'xmlparser :bind "get_attribute_count"
  :hash 3905245786)
 int)

(defgmethod
 (xmlparser+get-attribute-name :class 'xmlparser :bind "get_attribute_name"
  :hash 844755477)
 string (idx int))

(defgmethod
 (xmlparser+get-attribute-value :class 'xmlparser :bind "get_attribute_value"
  :hash 844755477)
 string (idx int))

(defgmethod
 (xmlparser+has-attribute :class 'xmlparser :bind "has_attribute" :hash
  3927539163)
 bool (name string))

(defgmethod
 (xmlparser+get-named-attribute-value :class 'xmlparser :bind
  "get_named_attribute_value" :hash 3135753539)
 string (name string))

(defgmethod
 (xmlparser+get-named-attribute-value-safe :class 'xmlparser :bind
  "get_named_attribute_value_safe" :hash 3135753539)
 string (name string))

(defgmethod
 (xmlparser+is-empty :class 'xmlparser :bind "is_empty" :hash 36873697) bool)

(defgmethod
 (xmlparser+get-current-line :class 'xmlparser :bind "get_current_line" :hash
  3905245786)
 int)

(defgmethod
 (xmlparser+skip-section :class 'xmlparser :bind "skip_section" :hash
  3218959716)
 :void)

(defgmethod (xmlparser+seek :class 'xmlparser :bind "seek" :hash 844576869)
 error (position int))

(defgmethod (xmlparser+open :class 'xmlparser :bind "open" :hash 166001499)
 error (file string))

(defgmethod
 (xmlparser+open-buffer :class 'xmlparser :bind "open_buffer" :hash 680677267)
 error (buffer packed-byte-array))