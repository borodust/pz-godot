(common-lisp:in-package :%godot)


(defgmethod
 (camera-server+set-monitoring-feeds :class 'camera-server :bind
  "set_monitoring_feeds" :hash 2586408642)
 :void (is-monitoring-feeds bool))

(defgmethod
 (camera-server+is-monitoring-feeds :class 'camera-server :bind
  "is_monitoring_feeds" :hash 36873697)
 bool)

(defgmethod
 (camera-server+get-feed :class 'camera-server :bind "get_feed" :hash
  361927068)
 camera-feed (index int))

(defgmethod
 (camera-server+get-feed-count :class 'camera-server :bind "get_feed_count"
  :hash 2455072627)
 int)

(defgmethod
 (camera-server+feeds :class 'camera-server :bind "feeds" :hash 2915620761)
 array)

(defgmethod
 (camera-server+add-feed :class 'camera-server :bind "add_feed" :hash
  3204782488)
 :void (feed camera-feed))

(defgmethod
 (camera-server+remove-feed :class 'camera-server :bind "remove_feed" :hash
  3204782488)
 :void (feed camera-feed))