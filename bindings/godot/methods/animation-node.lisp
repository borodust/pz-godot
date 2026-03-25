(common-lisp:in-package :%godot)


(defgmethod
 (animation-node+-get-child-nodes :class 'animation-node :bind
  "_get_child_nodes" :hash 3102165223 :virtual common-lisp:t)
 dictionary)

(defgmethod
 (animation-node+-get-parameter-list :class 'animation-node :bind
  "_get_parameter_list" :hash 3995934104 :virtual common-lisp:t)
 array)

(defgmethod
 (animation-node+-get-child-by-name :class 'animation-node :bind
  "_get_child_by_name" :hash 625644256 :virtual common-lisp:t)
 animation-node (name string-name))

(defgmethod
 (animation-node+-get-parameter-default-value :class 'animation-node :bind
  "_get_parameter_default_value" :hash 2760726917 :virtual common-lisp:t)
 variant (parameter string-name))

(defgmethod
 (animation-node+-is-parameter-read-only :class 'animation-node :bind
  "_is_parameter_read_only" :hash 2619796661 :virtual common-lisp:t)
 bool (parameter string-name))

(defgmethod
 (animation-node+-process :class 'animation-node :bind "_process" :hash
  2139827523 :virtual common-lisp:t)
 float (time float) (seek bool) (is-external-seeking bool) (test-only bool))

(defgmethod
 (animation-node+-get-caption :class 'animation-node :bind "_get_caption" :hash
  201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (animation-node+-has-filter :class 'animation-node :bind "_has_filter" :hash
  36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (animation-node+add-input :class 'animation-node :bind "add_input" :hash
  2323990056)
 bool (name string))

(defgmethod
 (animation-node+remove-input :class 'animation-node :bind "remove_input" :hash
  1286410249)
 :void (index int))

(defgmethod
 (animation-node+set-input-name :class 'animation-node :bind "set_input_name"
  :hash 215573526)
 bool (input int) (name string))

(defgmethod
 (animation-node+get-input-name :class 'animation-node :bind "get_input_name"
  :hash 844755477)
 string (input int))

(defgmethod
 (animation-node+get-input-count :class 'animation-node :bind "get_input_count"
  :hash 3905245786)
 int)

(defgmethod
 (animation-node+find-input :class 'animation-node :bind "find_input" :hash
  1321353865)
 int (name string))

(defgmethod
 (animation-node+set-filter-path :class 'animation-node :bind "set_filter_path"
  :hash 3868023870)
 :void (path node-path) (enable bool))

(defgmethod
 (animation-node+is-path-filtered :class 'animation-node :bind
  "is_path_filtered" :hash 861721659)
 bool (path node-path))

(defgmethod
 (animation-node+set-filter-enabled :class 'animation-node :bind
  "set_filter_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (animation-node+is-filter-enabled :class 'animation-node :bind
  "is_filter_enabled" :hash 36873697)
 bool)

(defgmethod
 (animation-node+get-processing-animation-tree-instance-id :class
  'animation-node :bind "get_processing_animation_tree_instance_id" :hash
  3905245786)
 int)

(defgmethod
 (animation-node+is-process-testing :class 'animation-node :bind
  "is_process_testing" :hash 36873697)
 bool)

(defgmethod
 (animation-node+blend-animation :class 'animation-node :bind "blend_animation"
  :hash 1630801826)
 :void (animation string-name) (time float) (delta float) (seeked bool)
 (is-external-seeking bool) (blend float) (looped-flag animation+looped-flag))

(defgmethod
 (animation-node+blend-node :class 'animation-node :bind "blend_node" :hash
  1746075988)
 float (name string-name) (node animation-node) (time float) (seek bool)
 (is-external-seeking bool) (blend float) (filter animation-node+filter-action)
 (sync bool) (test-only bool))

(defgmethod
 (animation-node+blend-input :class 'animation-node :bind "blend_input" :hash
  1361527350)
 float (input-index int) (time float) (seek bool) (is-external-seeking bool)
 (blend float) (filter animation-node+filter-action) (sync bool)
 (test-only bool))

(defgmethod
 (animation-node+set-parameter :class 'animation-node :bind "set_parameter"
  :hash 3776071444)
 :void (name string-name) (value variant))

(defgmethod
 (animation-node+get-parameter :class 'animation-node :bind "get_parameter"
  :hash 2760726917)
 variant (name string-name))