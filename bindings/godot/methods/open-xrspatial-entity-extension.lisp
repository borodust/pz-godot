(common-lisp:in-package :%godot)


(defgmethod
 (open-xrspatial-entity-extension+supports-capability :class
  'open-xrspatial-entity-extension :bind "supports_capability" :hash
  1940837202)
 bool (capability open-xrspatial-entity-extension+capability))

(defgmethod
 (open-xrspatial-entity-extension+supports-component-type :class
  'open-xrspatial-entity-extension :bind "supports_component_type" :hash
  26842779)
 bool (capability open-xrspatial-entity-extension+capability)
 (component-type open-xrspatial-entity-extension+component-type))

(defgmethod
 (open-xrspatial-entity-extension+create-spatial-context :class
  'open-xrspatial-entity-extension :bind "create_spatial_context" :hash
  1874506473)
 open-xrfuture-result (capability-configurations array)
 (next open-xrstructure-base) (user-callback callable))

(defgmethod
 (open-xrspatial-entity-extension+get-spatial-context-ready :class
  'open-xrspatial-entity-extension :bind "get_spatial_context_ready" :hash
  4155700596)
 bool (spatial-context rid))

(defgmethod
 (open-xrspatial-entity-extension+free-spatial-context :class
  'open-xrspatial-entity-extension :bind "free_spatial_context" :hash
  2722037293)
 :void (spatial-context rid))

(defgmethod
 (open-xrspatial-entity-extension+get-spatial-context-handle :class
  'open-xrspatial-entity-extension :bind "get_spatial_context_handle" :hash
  2198884583)
 int (spatial-context rid))

(defgmethod
 (open-xrspatial-entity-extension+discover-spatial-entities :class
  'open-xrspatial-entity-extension :bind "discover_spatial_entities" :hash
  2252833536)
 open-xrfuture-result (spatial-context rid)
 (component-types packed-int-64array) (next open-xrstructure-base)
 (user-callback callable))

(defgmethod
 (open-xrspatial-entity-extension+update-spatial-entities :class
  'open-xrspatial-entity-extension :bind "update_spatial_entities" :hash
  3446086438)
 rid (spatial-context rid) (entities array)
 (component-types packed-int-64array) (next open-xrstructure-base))

(defgmethod
 (open-xrspatial-entity-extension+free-spatial-snapshot :class
  'open-xrspatial-entity-extension :bind "free_spatial_snapshot" :hash
  2722037293)
 :void (spatial-snapshot rid))

(defgmethod
 (open-xrspatial-entity-extension+get-spatial-snapshot-handle :class
  'open-xrspatial-entity-extension :bind "get_spatial_snapshot_handle" :hash
  2198884583)
 int (spatial-snapshot rid))

(defgmethod
 (open-xrspatial-entity-extension+get-spatial-snapshot-context :class
  'open-xrspatial-entity-extension :bind "get_spatial_snapshot_context" :hash
  3814569979)
 rid (spatial-snapshot rid))

(defgmethod
 (open-xrspatial-entity-extension+query-snapshot :class
  'open-xrspatial-entity-extension :bind "query_snapshot" :hash 641015484)
 bool (spatial-snapshot rid) (component-data array)
 (next open-xrstructure-base))

(defgmethod
 (open-xrspatial-entity-extension+get-string :class
  'open-xrspatial-entity-extension :bind "get_string" :hash 1464764419)
 string (spatial-snapshot rid) (buffer-id int))

(defgmethod
 (open-xrspatial-entity-extension+get-uint8-buffer :class
  'open-xrspatial-entity-extension :bind "get_uint8_buffer" :hash 3570600051)
 packed-byte-array (spatial-snapshot rid) (buffer-id int))

(defgmethod
 (open-xrspatial-entity-extension+get-uint16-buffer :class
  'open-xrspatial-entity-extension :bind "get_uint16_buffer" :hash 3393655756)
 packed-int-32array (spatial-snapshot rid) (buffer-id int))

(defgmethod
 (open-xrspatial-entity-extension+get-uint32-buffer :class
  'open-xrspatial-entity-extension :bind "get_uint32_buffer" :hash 3393655756)
 packed-int-32array (spatial-snapshot rid) (buffer-id int))

(defgmethod
 (open-xrspatial-entity-extension+get-float-buffer :class
  'open-xrspatial-entity-extension :bind "get_float_buffer" :hash 2313216651)
 packed-float-32array (spatial-snapshot rid) (buffer-id int))

(defgmethod
 (open-xrspatial-entity-extension+get-vector2-buffer :class
  'open-xrspatial-entity-extension :bind "get_vector2_buffer" :hash 110850971)
 packed-vector-2array (spatial-snapshot rid) (buffer-id int))

(defgmethod
 (open-xrspatial-entity-extension+get-vector3-buffer :class
  'open-xrspatial-entity-extension :bind "get_vector3_buffer" :hash 1166453791)
 packed-vector-3array (spatial-snapshot rid) (buffer-id int))

(defgmethod
 (open-xrspatial-entity-extension+find-spatial-entity :class
  'open-xrspatial-entity-extension :bind "find_spatial_entity" :hash 937000113)
 rid (entity-id int))

(defgmethod
 (open-xrspatial-entity-extension+add-spatial-entity :class
  'open-xrspatial-entity-extension :bind "add_spatial_entity" :hash 2256026069)
 rid (spatial-context rid) (entity-id int) (entity int))

(defgmethod
 (open-xrspatial-entity-extension+make-spatial-entity :class
  'open-xrspatial-entity-extension :bind "make_spatial_entity" :hash
  2233757277)
 rid (spatial-context rid) (entity-id int))

(defgmethod
 (open-xrspatial-entity-extension+get-spatial-entity-id :class
  'open-xrspatial-entity-extension :bind "get_spatial_entity_id" :hash
  2198884583)
 int (entity rid))

(defgmethod
 (open-xrspatial-entity-extension+get-spatial-entity-context :class
  'open-xrspatial-entity-extension :bind "get_spatial_entity_context" :hash
  3814569979)
 rid (entity rid))

(defgmethod
 (open-xrspatial-entity-extension+free-spatial-entity :class
  'open-xrspatial-entity-extension :bind "free_spatial_entity" :hash
  2722037293)
 :void (entity rid))