(common-lisp:in-package :%godot)


(defgmethod
 (undo-redo+create-action :class 'undo-redo :bind "create_action" :hash
  3171901514)
 :void (name string) (merge-mode undo-redo+merge-mode) (backward-undo-ops bool))

(defgmethod
 (undo-redo+commit-action :class 'undo-redo :bind "commit_action" :hash
  3216645846)
 :void (execute bool))

(defgmethod
 (undo-redo+is-committing-action :class 'undo-redo :bind "is_committing_action"
  :hash 36873697)
 bool)

(defgmethod
 (undo-redo+add-do-method :class 'undo-redo :bind "add_do_method" :hash
  1611583062)
 :void (callable callable))

(defgmethod
 (undo-redo+add-undo-method :class 'undo-redo :bind "add_undo_method" :hash
  1611583062)
 :void (callable callable))

(defgmethod
 (undo-redo+add-do-property :class 'undo-redo :bind "add_do_property" :hash
  1017172818)
 :void (object object) (property string-name) (value variant))

(defgmethod
 (undo-redo+add-undo-property :class 'undo-redo :bind "add_undo_property" :hash
  1017172818)
 :void (object object) (property string-name) (value variant))

(defgmethod
 (undo-redo+add-do-reference :class 'undo-redo :bind "add_do_reference" :hash
  3975164845)
 :void (object object))

(defgmethod
 (undo-redo+add-undo-reference :class 'undo-redo :bind "add_undo_reference"
  :hash 3975164845)
 :void (object object))

(defgmethod
 (undo-redo+start-force-keep-in-merge-ends :class 'undo-redo :bind
  "start_force_keep_in_merge_ends" :hash 3218959716)
 :void)

(defgmethod
 (undo-redo+end-force-keep-in-merge-ends :class 'undo-redo :bind
  "end_force_keep_in_merge_ends" :hash 3218959716)
 :void)

(defgmethod
 (undo-redo+get-history-count :class 'undo-redo :bind "get_history_count" :hash
  2455072627)
 int)

(defgmethod
 (undo-redo+get-current-action :class 'undo-redo :bind "get_current_action"
  :hash 2455072627)
 int)

(defgmethod
 (undo-redo+get-action-name :class 'undo-redo :bind "get_action_name" :hash
  990163283)
 string (id int))

(defgmethod
 (undo-redo+clear-history :class 'undo-redo :bind "clear_history" :hash
  3216645846)
 :void (increase-version bool))

(defgmethod
 (undo-redo+get-current-action-name :class 'undo-redo :bind
  "get_current_action_name" :hash 201670096)
 string)

(defgmethod
 (undo-redo+has-undo :class 'undo-redo :bind "has_undo" :hash 36873697) bool)

(defgmethod
 (undo-redo+has-redo :class 'undo-redo :bind "has_redo" :hash 36873697) bool)

(defgmethod
 (undo-redo+get-version :class 'undo-redo :bind "get_version" :hash 3905245786)
 int)

(defgmethod
 (undo-redo+set-max-steps :class 'undo-redo :bind "set_max_steps" :hash
  1286410249)
 :void (max-steps int))

(defgmethod
 (undo-redo+get-max-steps :class 'undo-redo :bind "get_max_steps" :hash
  3905245786)
 int)

(defgmethod (undo-redo+redo :class 'undo-redo :bind "redo" :hash 2240911060)
 bool)

(defgmethod (undo-redo+undo :class 'undo-redo :bind "undo" :hash 2240911060)
 bool)