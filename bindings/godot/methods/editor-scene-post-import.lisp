(common-lisp:in-package :%godot)


(defgmethod
 (editor-scene-post-import+%post-import :class 'editor-scene-post-import :bind
  "_post_import" :hash 134930648 :virtual common-lisp:t)
 object (scene node))

(defgmethod
 (editor-scene-post-import+get-source-file :class 'editor-scene-post-import
  :bind "get_source_file" :hash 201670096)
 string)