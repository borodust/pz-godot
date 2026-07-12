(common-lisp:in-package :%godot)


(defgmethod
 (editor-vcsinterface+%initialize :class 'editor-vcsinterface :bind
  "_initialize" :hash 2323990056 :virtual common-lisp:t)
 bool (project-path string))

(defgmethod
 (editor-vcsinterface+%set-credentials :class 'editor-vcsinterface :bind
  "_set_credentials" :hash 1336744649 :virtual common-lisp:t)
 :void (username string) (password string) (ssh-public-key-path string)
 (ssh-private-key-path string) (ssh-passphrase string))

(defgmethod
 (editor-vcsinterface+%get-modified-files-data :class 'editor-vcsinterface
  :bind "_get_modified_files_data" :hash 2915620761 :virtual common-lisp:t)
 array)

(defgmethod
 (editor-vcsinterface+%stage-file :class 'editor-vcsinterface :bind
  "_stage_file" :hash 83702148 :virtual common-lisp:t)
 :void (file-path string))

(defgmethod
 (editor-vcsinterface+%unstage-file :class 'editor-vcsinterface :bind
  "_unstage_file" :hash 83702148 :virtual common-lisp:t)
 :void (file-path string))

(defgmethod
 (editor-vcsinterface+%discard-file :class 'editor-vcsinterface :bind
  "_discard_file" :hash 83702148 :virtual common-lisp:t)
 :void (file-path string))

(defgmethod
 (editor-vcsinterface+%commit :class 'editor-vcsinterface :bind "_commit" :hash
  2678287736 :virtual common-lisp:t)
 :void (msg string) (amend bool))

(defgmethod
 (editor-vcsinterface+%allow-amends :class 'editor-vcsinterface :bind
  "_allow_amends" :hash 2240911060 :virtual common-lisp:t)
 bool)

(defgmethod
 (editor-vcsinterface+%get-diff :class 'editor-vcsinterface :bind "_get_diff"
  :hash 1366379175 :virtual common-lisp:t)
 array (identifier string) (area int))

(defgmethod
 (editor-vcsinterface+%shut-down :class 'editor-vcsinterface :bind "_shut_down"
  :hash 2240911060 :virtual common-lisp:t)
 bool)

(defgmethod
 (editor-vcsinterface+%get-vcs-name :class 'editor-vcsinterface :bind
  "_get_vcs_name" :hash 2841200299 :virtual common-lisp:t)
 string)

(defgmethod
 (editor-vcsinterface+%get-previous-commits :class 'editor-vcsinterface :bind
  "_get_previous_commits" :hash 1171824711 :virtual common-lisp:t)
 array (max-commits int))

(defgmethod
 (editor-vcsinterface+%get-branch-list :class 'editor-vcsinterface :bind
  "_get_branch_list" :hash 2915620761 :virtual common-lisp:t)
 array)

(defgmethod
 (editor-vcsinterface+%get-remotes :class 'editor-vcsinterface :bind
  "_get_remotes" :hash 2915620761 :virtual common-lisp:t)
 array)

(defgmethod
 (editor-vcsinterface+%create-branch :class 'editor-vcsinterface :bind
  "_create_branch" :hash 83702148 :virtual common-lisp:t)
 :void (branch-name string))

(defgmethod
 (editor-vcsinterface+%remove-branch :class 'editor-vcsinterface :bind
  "_remove_branch" :hash 83702148 :virtual common-lisp:t)
 :void (branch-name string))

(defgmethod
 (editor-vcsinterface+%create-remote :class 'editor-vcsinterface :bind
  "_create_remote" :hash 3186203200 :virtual common-lisp:t)
 :void (remote-name string) (remote-url string))

(defgmethod
 (editor-vcsinterface+%remove-remote :class 'editor-vcsinterface :bind
  "_remove_remote" :hash 83702148 :virtual common-lisp:t)
 :void (remote-name string))

(defgmethod
 (editor-vcsinterface+%get-current-branch-name :class 'editor-vcsinterface
  :bind "_get_current_branch_name" :hash 2841200299 :virtual common-lisp:t)
 string)

(defgmethod
 (editor-vcsinterface+%checkout-branch :class 'editor-vcsinterface :bind
  "_checkout_branch" :hash 2323990056 :virtual common-lisp:t)
 bool (branch-name string))

(defgmethod
 (editor-vcsinterface+%pull :class 'editor-vcsinterface :bind "_pull" :hash
  83702148 :virtual common-lisp:t)
 :void (remote string))

(defgmethod
 (editor-vcsinterface+%push :class 'editor-vcsinterface :bind "_push" :hash
  2678287736 :virtual common-lisp:t)
 :void (remote string) (force bool))

(defgmethod
 (editor-vcsinterface+%fetch :class 'editor-vcsinterface :bind "_fetch" :hash
  83702148 :virtual common-lisp:t)
 :void (remote string))

(defgmethod
 (editor-vcsinterface+%get-line-diff :class 'editor-vcsinterface :bind
  "_get_line_diff" :hash 2796572089 :virtual common-lisp:t)
 array (file-path string) (text string))

(defgmethod
 (editor-vcsinterface+create-diff-line :class 'editor-vcsinterface :bind
  "create_diff_line" :hash 2901184053)
 dictionary (new-line-no int) (old-line-no int) (content string)
 (status string))

(defgmethod
 (editor-vcsinterface+create-diff-hunk :class 'editor-vcsinterface :bind
  "create_diff_hunk" :hash 3784842090)
 dictionary (old-start int) (new-start int) (old-lines int) (new-lines int))

(defgmethod
 (editor-vcsinterface+create-diff-file :class 'editor-vcsinterface :bind
  "create_diff_file" :hash 2723227684)
 dictionary (new-file string) (old-file string))

(defgmethod
 (editor-vcsinterface+create-commit :class 'editor-vcsinterface :bind
  "create_commit" :hash 1075983584)
 dictionary (msg string) (author string) (id string) (unix-timestamp int)
 (offset-minutes int))

(defgmethod
 (editor-vcsinterface+create-status-file :class 'editor-vcsinterface :bind
  "create_status_file" :hash 1083471673)
 dictionary (file-path string) (change-type editor-vcsinterface+change-type)
 (area editor-vcsinterface+tree-area))

(defgmethod
 (editor-vcsinterface+add-diff-hunks-into-diff-file :class 'editor-vcsinterface
  :bind "add_diff_hunks_into_diff_file" :hash 4015243225)
 dictionary (diff-file dictionary) (diff-hunks array))

(defgmethod
 (editor-vcsinterface+add-line-diffs-into-diff-hunk :class 'editor-vcsinterface
  :bind "add_line_diffs_into_diff_hunk" :hash 4015243225)
 dictionary (diff-hunk dictionary) (line-diffs array))

(defgmethod
 (editor-vcsinterface+popup-error :class 'editor-vcsinterface :bind
  "popup_error" :hash 83702148)
 :void (msg string))