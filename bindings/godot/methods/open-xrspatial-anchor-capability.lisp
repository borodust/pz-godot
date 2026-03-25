(common-lisp:in-package :%godot)


(defgmethod
 (open-xrspatial-anchor-capability+is-spatial-anchor-supported :class
  'open-xrspatial-anchor-capability :bind "is_spatial_anchor_supported" :hash
  2240911060)
 bool)

(defgmethod
 (open-xrspatial-anchor-capability+is-spatial-persistence-supported :class
  'open-xrspatial-anchor-capability :bind "is_spatial_persistence_supported"
  :hash 2240911060)
 bool)

(defgmethod
 (open-xrspatial-anchor-capability+is-persistence-scope-supported :class
  'open-xrspatial-anchor-capability :bind "is_persistence_scope_supported"
  :hash 3651771626)
 bool (scope open-xrspatial-anchor-capability+persistence-scope))

(defgmethod
 (open-xrspatial-anchor-capability+create-persistence-context :class
  'open-xrspatial-anchor-capability :bind "create_persistence_context" :hash
  856276630)
 open-xrfuture-result
 (scope open-xrspatial-anchor-capability+persistence-scope)
 (user-callback callable))

(defgmethod
 (open-xrspatial-anchor-capability+get-persistence-context-handle :class
  'open-xrspatial-anchor-capability :bind "get_persistence_context_handle"
  :hash 2198884583)
 int (persistence-context rid))

(defgmethod
 (open-xrspatial-anchor-capability+free-persistence-context :class
  'open-xrspatial-anchor-capability :bind "free_persistence_context" :hash
  2722037293)
 :void (persistence-context rid))

(defgmethod
 (open-xrspatial-anchor-capability+create-new-anchor :class
  'open-xrspatial-anchor-capability :bind "create_new_anchor" :hash 607100373)
 open-xranchor-tracker (transform transform-3d) (spatial-context rid))

(defgmethod
 (open-xrspatial-anchor-capability+remove-anchor :class
  'open-xrspatial-anchor-capability :bind "remove_anchor" :hash 3579451518)
 :void (anchor-tracker open-xranchor-tracker))

(defgmethod
 (open-xrspatial-anchor-capability+persist-anchor :class
  'open-xrspatial-anchor-capability :bind "persist_anchor" :hash 4244202513)
 open-xrfuture-result (anchor-tracker open-xranchor-tracker)
 (persistence-context rid) (user-callback callable))

(defgmethod
 (open-xrspatial-anchor-capability+unpersist-anchor :class
  'open-xrspatial-anchor-capability :bind "unpersist_anchor" :hash 4244202513)
 open-xrfuture-result (anchor-tracker open-xranchor-tracker)
 (persistence-context rid) (user-callback callable))