(common-lisp:in-package :%godot)


(defgmethod
 (editor-feature-profile+set-disable-class :class 'editor-feature-profile :bind
  "set_disable_class" :hash 2524380260)
 :void (class-name string-name) (disable bool))

(defgmethod
 (editor-feature-profile+is-class-disabled :class 'editor-feature-profile :bind
  "is_class_disabled" :hash 2619796661)
 bool (class-name string-name))

(defgmethod
 (editor-feature-profile+set-disable-class-editor :class
  'editor-feature-profile :bind "set_disable_class_editor" :hash 2524380260)
 :void (class-name string-name) (disable bool))

(defgmethod
 (editor-feature-profile+is-class-editor-disabled :class
  'editor-feature-profile :bind "is_class_editor_disabled" :hash 2619796661)
 bool (class-name string-name))

(defgmethod
 (editor-feature-profile+set-disable-class-property :class
  'editor-feature-profile :bind "set_disable_class_property" :hash 865197084)
 :void (class-name string-name) (property string-name) (disable bool))

(defgmethod
 (editor-feature-profile+is-class-property-disabled :class
  'editor-feature-profile :bind "is_class_property_disabled" :hash 471820014)
 bool (class-name string-name) (property string-name))

(defgmethod
 (editor-feature-profile+set-disable-feature :class 'editor-feature-profile
  :bind "set_disable_feature" :hash 1884871044)
 :void (feature editor-feature-profile+feature) (disable bool))

(defgmethod
 (editor-feature-profile+is-feature-disabled :class 'editor-feature-profile
  :bind "is_feature_disabled" :hash 2974403161)
 bool (feature editor-feature-profile+feature))

(defgmethod
 (editor-feature-profile+get-feature-name :class 'editor-feature-profile :bind
  "get_feature_name" :hash 3401335809)
 string (feature editor-feature-profile+feature))

(defgmethod
 (editor-feature-profile+save-to-file :class 'editor-feature-profile :bind
  "save_to_file" :hash 166001499)
 error (path string))

(defgmethod
 (editor-feature-profile+load-from-file :class 'editor-feature-profile :bind
  "load_from_file" :hash 166001499)
 error (path string))