(common-lisp:in-package :%godot)


(defgmethod
 (editor-resource-preview+queue-resource-preview :class
  'editor-resource-preview :bind "queue_resource_preview" :hash 233177534)
 :void (path string) (receiver object) (receiver-func string-name)
 (userdata variant))

(defgmethod
 (editor-resource-preview+queue-edited-resource-preview :class
  'editor-resource-preview :bind "queue_edited_resource_preview" :hash
  1608376650)
 :void (resource resource) (receiver object) (receiver-func string-name)
 (userdata variant))

(defgmethod
 (editor-resource-preview+add-preview-generator :class 'editor-resource-preview
  :bind "add_preview_generator" :hash 332288124)
 :void (generator editor-resource-preview-generator))

(defgmethod
 (editor-resource-preview+remove-preview-generator :class
  'editor-resource-preview :bind "remove_preview_generator" :hash 332288124)
 :void (generator editor-resource-preview-generator))

(defgmethod
 (editor-resource-preview+check-for-invalidation :class
  'editor-resource-preview :bind "check_for_invalidation" :hash 83702148)
 :void (path string))