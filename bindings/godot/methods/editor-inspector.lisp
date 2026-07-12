(common-lisp:in-package :%godot)


(defgmethod
 (editor-inspector+edit :class 'editor-inspector :bind "edit" :hash 3975164845)
 :void (object object))

(defgmethod
 (editor-inspector+get-selected-path :class 'editor-inspector :bind
  "get_selected_path" :hash 201670096)
 string)

(defgmethod
 (editor-inspector+get-edited-object :class 'editor-inspector :bind
  "get_edited_object" :hash 2050059866)
 object)

(defgmethod
 (editor-inspector+collapse-all-folding :class 'editor-inspector :bind
  "collapse_all_folding" :hash 3218959716)
 :void)

(defgmethod
 (editor-inspector+expand-all-folding :class 'editor-inspector :bind
  "expand_all_folding" :hash 3218959716)
 :void)

(defgmethod
 (editor-inspector+expand-revertable :class 'editor-inspector :bind
  "expand_revertable" :hash 3218959716)
 :void)

(defgmethod
 (editor-inspector+instantiate-property-editor :class 'editor-inspector :bind
  "instantiate_property_editor" :hash 1429914152 :static common-lisp:t)
 editor-property (object object) (type variant+type) (path string)
 (hint property-hint) (hint-text string) (usage int) (wide bool))

(defgmethod
 (editor-inspector+create-default-inspector :class 'editor-inspector :bind
  "create_default_inspector" :hash 2419746798 :static common-lisp:t)
 editor-inspector (filter-line-edit line-edit))