(common-lisp:in-package :%godot)


(defgmethod (object+get-class :class 'object :bind "get_class" :hash 201670096)
 string)

(defgmethod (object+is-class :class 'object :bind "is_class" :hash 2619796661)
 bool (class string-name))

(defgmethod (object+set :class 'object :bind "set" :hash 3776071444) :void
 (property string-name) (value variant))

(defgmethod (object+get :class 'object :bind "get" :hash 2760726917) variant
 (property string-name))

(defgmethod
 (object+set-indexed :class 'object :bind "set_indexed" :hash 3500910842) :void
 (property-path node-path) (value variant))

(defgmethod
 (object+get-indexed :class 'object :bind "get_indexed" :hash 4006125091)
 variant (property-path node-path))

(defgmethod
 (object+get-property-list :class 'object :bind "get_property_list" :hash
  3995934104)
 array)

(defgmethod
 (object+get-method-list :class 'object :bind "get_method_list" :hash
  3995934104)
 array)

(defgmethod
 (object+property-can-revert :class 'object :bind "property_can_revert" :hash
  2619796661)
 bool (property string-name))

(defgmethod
 (object+property-get-revert :class 'object :bind "property_get_revert" :hash
  2760726917)
 variant (property string-name))

(defgmethod
 (object+notification :class 'object :bind "notification" :hash 4023243586)
 :void (what int) (reversed bool))

(defgmethod
 (object+to-string :class 'object :bind "to_string" :hash 2841200299) string)

(defgmethod
 (object+get-instance-id :class 'object :bind "get_instance_id" :hash
  3905245786)
 int)

(defgmethod
 (object+set-script :class 'object :bind "set_script" :hash 1114965689) :void
 (script variant))

(defgmethod
 (object+get-script :class 'object :bind "get_script" :hash 1214101251) variant)

(defgmethod (object+set-meta :class 'object :bind "set_meta" :hash 3776071444)
 :void (name string-name) (value variant))

(defgmethod
 (object+remove-meta :class 'object :bind "remove_meta" :hash 3304788590) :void
 (name string-name))

(defgmethod (object+get-meta :class 'object :bind "get_meta" :hash 3990617847)
 variant (name string-name) (default variant))

(defgmethod (object+has-meta :class 'object :bind "has_meta" :hash 2619796661)
 bool (name string-name))

(defgmethod
 (object+get-meta-list :class 'object :bind "get_meta_list" :hash 3995934104)
 array)

(defgmethod
 (object+add-user-signal :class 'object :bind "add_user_signal" :hash 85656714)
 :void (signal string) (arguments array))

(defgmethod
 (object+has-user-signal :class 'object :bind "has_user_signal" :hash
  2619796661)
 bool (signal string-name))

(defgmethod
 (object+remove-user-signal :class 'object :bind "remove_user_signal" :hash
  3304788590)
 :void (signal string-name))

(defgmethod
 (object+emit-signal :class 'object :bind "emit_signal" :hash 4047867050
  :vararg common-lisp:t)
 error (signal string-name))

(defgmethod
 (object+call :class 'object :bind "call" :hash 3400424181 :vararg
  common-lisp:t)
 variant (method string-name))

(defgmethod
 (object+call-deferred :class 'object :bind "call_deferred" :hash 3400424181
  :vararg common-lisp:t)
 variant (method string-name))

(defgmethod
 (object+set-deferred :class 'object :bind "set_deferred" :hash 3776071444)
 :void (property string-name) (value variant))

(defgmethod (object+callv :class 'object :bind "callv" :hash 1260104456)
 variant (method string-name) (arg-array array))

(defgmethod
 (object+has-method :class 'object :bind "has_method" :hash 2619796661) bool
 (method string-name))

(defgmethod
 (object+get-method-argument-count :class 'object :bind
  "get_method_argument_count" :hash 2458036349)
 int (method string-name))

(defgmethod
 (object+has-signal :class 'object :bind "has_signal" :hash 2619796661) bool
 (signal string-name))

(defgmethod
 (object+get-signal-list :class 'object :bind "get_signal_list" :hash
  3995934104)
 array)

(defgmethod
 (object+get-signal-connection-list :class 'object :bind
  "get_signal_connection_list" :hash 3147814860)
 array (signal string-name))

(defgmethod
 (object+get-incoming-connections :class 'object :bind
  "get_incoming_connections" :hash 3995934104)
 array)

(defgmethod (object+connect :class 'object :bind "connect" :hash 1518946055)
 error (signal string-name) (callable callable) (flags int))

(defgmethod
 (object+disconnect :class 'object :bind "disconnect" :hash 1874754934) :void
 (signal string-name) (callable callable))

(defgmethod
 (object+is-connected :class 'object :bind "is_connected" :hash 768136979) bool
 (signal string-name) (callable callable))

(defgmethod
 (object+has-connections :class 'object :bind "has_connections" :hash
  2619796661)
 bool (signal string-name))

(defgmethod
 (object+set-block-signals :class 'object :bind "set_block_signals" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (object+is-blocking-signals :class 'object :bind "is_blocking_signals" :hash
  36873697)
 bool)

(defgmethod
 (object+notify-property-list-changed :class 'object :bind
  "notify_property_list_changed" :hash 3218959716)
 :void)

(defgmethod
 (object+set-message-translation :class 'object :bind "set_message_translation"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (object+can-translate-messages :class 'object :bind "can_translate_messages"
  :hash 36873697)
 bool)

(defgmethod (object+tr :class 'object :bind "tr" :hash 1195764410) string
 (message string-name) (context string-name))

(defgmethod (object+tr-n :class 'object :bind "tr_n" :hash 162698058) string
 (message string-name) (plural-message string-name) (n int)
 (context string-name))

(defgmethod
 (object+get-translation-domain :class 'object :bind "get_translation_domain"
  :hash 2002593661)
 string-name)

(defgmethod
 (object+set-translation-domain :class 'object :bind "set_translation_domain"
  :hash 3304788590)
 :void (domain string-name))

(defgmethod
 (object+is-queued-for-deletion :class 'object :bind "is_queued_for_deletion"
  :hash 36873697)
 bool)

(defgmethod
 (object+cancel-free :class 'object :bind "cancel_free" :hash 3218959716) :void)