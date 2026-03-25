(common-lisp:in-package :%godot)


(defgmethod
 (editor-inspector-plugin+-can-handle :class 'editor-inspector-plugin :bind
  "_can_handle" :hash 397768994 :virtual common-lisp:t)
 bool (object object))

(defgmethod
 (editor-inspector-plugin+-parse-begin :class 'editor-inspector-plugin :bind
  "_parse_begin" :hash 3975164845 :virtual common-lisp:t)
 :void (object object))

(defgmethod
 (editor-inspector-plugin+-parse-category :class 'editor-inspector-plugin :bind
  "_parse_category" :hash 357144787 :virtual common-lisp:t)
 :void (object object) (category string))

(defgmethod
 (editor-inspector-plugin+-parse-group :class 'editor-inspector-plugin :bind
  "_parse_group" :hash 357144787 :virtual common-lisp:t)
 :void (object object) (group string))

(defgmethod
 (editor-inspector-plugin+-parse-property :class 'editor-inspector-plugin :bind
  "_parse_property" :hash 1087679910 :virtual common-lisp:t)
 bool (object object) (type variant+type) (name string)
 (hint-type property-hint) (hint-string string)
 (usage-flags property-usage-flags) (wide bool))

(defgmethod
 (editor-inspector-plugin+-parse-end :class 'editor-inspector-plugin :bind
  "_parse_end" :hash 3975164845 :virtual common-lisp:t)
 :void (object object))

(defgmethod
 (editor-inspector-plugin+add-custom-control :class 'editor-inspector-plugin
  :bind "add_custom_control" :hash 1496901182)
 :void (control control))

(defgmethod
 (editor-inspector-plugin+add-property-editor :class 'editor-inspector-plugin
  :bind "add_property_editor" :hash 2042698479)
 :void (property string) (editor control) (add-to-end bool) (label string))

(defgmethod
 (editor-inspector-plugin+add-property-editor-for-multiple-properties :class
  'editor-inspector-plugin :bind "add_property_editor_for_multiple_properties"
  :hash 788598683)
 :void (label string) (properties packed-string-array) (editor control))