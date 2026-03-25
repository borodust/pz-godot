(common-lisp:in-package :%godot)


(defgmethod
 (editor-undo-redo-manager+create-action :class 'editor-undo-redo-manager :bind
  "create_action" :hash 796197507)
 :void (name string) (merge-mode undo-redo+merge-mode) (custom-context object)
 (backward-undo-ops bool) (mark-unsaved bool))

(defgmethod
 (editor-undo-redo-manager+commit-action :class 'editor-undo-redo-manager :bind
  "commit_action" :hash 3216645846)
 :void (execute bool))

(defgmethod
 (editor-undo-redo-manager+is-committing-action :class
  'editor-undo-redo-manager :bind "is_committing_action" :hash 36873697)
 bool)

(defgmethod
 (editor-undo-redo-manager+force-fixed-history :class 'editor-undo-redo-manager
  :bind "force_fixed_history" :hash 3218959716)
 :void)

(defgmethod
 (editor-undo-redo-manager+add-do-method :class 'editor-undo-redo-manager :bind
  "add_do_method" :hash 1517810467 :vararg common-lisp:t)
 :void (object object) (method string-name))

(defgmethod
 (editor-undo-redo-manager+add-undo-method :class 'editor-undo-redo-manager
  :bind "add_undo_method" :hash 1517810467 :vararg common-lisp:t)
 :void (object object) (method string-name))

(defgmethod
 (editor-undo-redo-manager+add-do-property :class 'editor-undo-redo-manager
  :bind "add_do_property" :hash 1017172818)
 :void (object object) (property string-name) (value variant))

(defgmethod
 (editor-undo-redo-manager+add-undo-property :class 'editor-undo-redo-manager
  :bind "add_undo_property" :hash 1017172818)
 :void (object object) (property string-name) (value variant))

(defgmethod
 (editor-undo-redo-manager+add-do-reference :class 'editor-undo-redo-manager
  :bind "add_do_reference" :hash 3975164845)
 :void (object object))

(defgmethod
 (editor-undo-redo-manager+add-undo-reference :class 'editor-undo-redo-manager
  :bind "add_undo_reference" :hash 3975164845)
 :void (object object))

(defgmethod
 (editor-undo-redo-manager+get-object-history-id :class
  'editor-undo-redo-manager :bind "get_object_history_id" :hash 1107568780)
 int (object object))

(defgmethod
 (editor-undo-redo-manager+get-history-undo-redo :class
  'editor-undo-redo-manager :bind "get_history_undo_redo" :hash 2417974513)
 undo-redo (id int))

(defgmethod
 (editor-undo-redo-manager+clear-history :class 'editor-undo-redo-manager :bind
  "clear_history" :hash 2020603371)
 :void (id int) (increase-version bool))