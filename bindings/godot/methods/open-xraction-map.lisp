(common-lisp:in-package :%godot)


(defgmethod
 (open-xraction-map+set-action-sets :class 'open-xraction-map :bind
  "set_action_sets" :hash 381264803)
 :void (action-sets array))

(defgmethod
 (open-xraction-map+get-action-sets :class 'open-xraction-map :bind
  "get_action_sets" :hash 3995934104)
 array)

(defgmethod
 (open-xraction-map+get-action-set-count :class 'open-xraction-map :bind
  "get_action_set_count" :hash 3905245786)
 int)

(defgmethod
 (open-xraction-map+find-action-set :class 'open-xraction-map :bind
  "find_action_set" :hash 1888809267)
 open-xraction-set (name string))

(defgmethod
 (open-xraction-map+get-action-set :class 'open-xraction-map :bind
  "get_action_set" :hash 1789580336)
 open-xraction-set (idx int))

(defgmethod
 (open-xraction-map+add-action-set :class 'open-xraction-map :bind
  "add_action_set" :hash 2093310581)
 :void (action-set open-xraction-set))

(defgmethod
 (open-xraction-map+remove-action-set :class 'open-xraction-map :bind
  "remove_action_set" :hash 2093310581)
 :void (action-set open-xraction-set))

(defgmethod
 (open-xraction-map+set-interaction-profiles :class 'open-xraction-map :bind
  "set_interaction_profiles" :hash 381264803)
 :void (interaction-profiles array))

(defgmethod
 (open-xraction-map+get-interaction-profiles :class 'open-xraction-map :bind
  "get_interaction_profiles" :hash 3995934104)
 array)

(defgmethod
 (open-xraction-map+get-interaction-profile-count :class 'open-xraction-map
  :bind "get_interaction_profile_count" :hash 3905245786)
 int)

(defgmethod
 (open-xraction-map+find-interaction-profile :class 'open-xraction-map :bind
  "find_interaction_profile" :hash 3095875538)
 open-xrinteraction-profile (name string))

(defgmethod
 (open-xraction-map+get-interaction-profile :class 'open-xraction-map :bind
  "get_interaction_profile" :hash 2546151210)
 open-xrinteraction-profile (idx int))

(defgmethod
 (open-xraction-map+add-interaction-profile :class 'open-xraction-map :bind
  "add_interaction_profile" :hash 2697953512)
 :void (interaction-profile open-xrinteraction-profile))

(defgmethod
 (open-xraction-map+remove-interaction-profile :class 'open-xraction-map :bind
  "remove_interaction_profile" :hash 2697953512)
 :void (interaction-profile open-xrinteraction-profile))

(defgmethod
 (open-xraction-map+create-default-action-sets :class 'open-xraction-map :bind
  "create_default_action_sets" :hash 3218959716)
 :void)