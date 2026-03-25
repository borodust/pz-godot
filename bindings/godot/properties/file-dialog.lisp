(common-lisp:in-package :%godot)


(defgproperty file-dialog+mode-overrides-title 'file-dialog :get
 'file-dialog+is-mode-overriding-title :set
 'file-dialog+set-mode-overrides-title)

(defgproperty file-dialog+file-mode 'file-dialog :get
 'file-dialog+get-file-mode :set 'file-dialog+set-file-mode)

(defgproperty file-dialog+display-mode 'file-dialog :get
 'file-dialog+get-display-mode :set 'file-dialog+set-display-mode)

(defgproperty file-dialog+access 'file-dialog :get 'file-dialog+get-access :set
 'file-dialog+set-access)

(defgproperty file-dialog+root-subfolder 'file-dialog :get
 'file-dialog+get-root-subfolder :set 'file-dialog+set-root-subfolder)

(defgproperty file-dialog+filters 'file-dialog :get 'file-dialog+get-filters
 :set 'file-dialog+set-filters)

(defgproperty file-dialog+filename-filter 'file-dialog :get
 'file-dialog+get-filename-filter :set 'file-dialog+set-filename-filter)

(defgproperty file-dialog+show-hidden-files 'file-dialog :get
 'file-dialog+is-showing-hidden-files :set 'file-dialog+set-show-hidden-files)

(defgproperty file-dialog+use-native-dialog 'file-dialog :get
 'file-dialog+get-use-native-dialog :set 'file-dialog+set-use-native-dialog)

(defgproperty file-dialog+option-count 'file-dialog :get
 'file-dialog+get-option-count :set 'file-dialog+set-option-count)

(defgproperty file-dialog+hidden-files-toggle-enabled 'file-dialog :index 0
 :get 'file-dialog+is-customization-flag-enabled :set
 'file-dialog+set-customization-flag-enabled)

(defgproperty file-dialog+file-filter-toggle-enabled 'file-dialog :index 2 :get
 'file-dialog+is-customization-flag-enabled :set
 'file-dialog+set-customization-flag-enabled)

(defgproperty file-dialog+file-sort-options-enabled 'file-dialog :index 3 :get
 'file-dialog+is-customization-flag-enabled :set
 'file-dialog+set-customization-flag-enabled)

(defgproperty file-dialog+folder-creation-enabled 'file-dialog :index 1 :get
 'file-dialog+is-customization-flag-enabled :set
 'file-dialog+set-customization-flag-enabled)

(defgproperty file-dialog+favorites-enabled 'file-dialog :index 4 :get
 'file-dialog+is-customization-flag-enabled :set
 'file-dialog+set-customization-flag-enabled)

(defgproperty file-dialog+recent-list-enabled 'file-dialog :index 5 :get
 'file-dialog+is-customization-flag-enabled :set
 'file-dialog+set-customization-flag-enabled)

(defgproperty file-dialog+layout-toggle-enabled 'file-dialog :index 6 :get
 'file-dialog+is-customization-flag-enabled :set
 'file-dialog+set-customization-flag-enabled)

(defgproperty file-dialog+overwrite-warning-enabled 'file-dialog :index 7 :get
 'file-dialog+is-customization-flag-enabled :set
 'file-dialog+set-customization-flag-enabled)

(defgproperty file-dialog+deleting-enabled 'file-dialog :index 8 :get
 'file-dialog+is-customization-flag-enabled :set
 'file-dialog+set-customization-flag-enabled)

(defgproperty file-dialog+current-dir 'file-dialog :get
 'file-dialog+get-current-dir :set 'file-dialog+set-current-dir)

(defgproperty file-dialog+current-file 'file-dialog :get
 'file-dialog+get-current-file :set 'file-dialog+set-current-file)

(defgproperty file-dialog+current-path 'file-dialog :get
 'file-dialog+get-current-path :set 'file-dialog+set-current-path)