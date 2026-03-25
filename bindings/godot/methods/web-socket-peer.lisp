(common-lisp:in-package :%godot)


(defgmethod
 (web-socket-peer+connect-to-url :class 'web-socket-peer :bind "connect_to_url"
  :hash 1966198364)
 error (url string) (tls-client-options tlsoptions))

(defgmethod
 (web-socket-peer+accept-stream :class 'web-socket-peer :bind "accept_stream"
  :hash 255125695)
 error (stream stream-peer))

(defgmethod
 (web-socket-peer+send :class 'web-socket-peer :bind "send" :hash 2780360567)
 error (message packed-byte-array) (write-mode web-socket-peer+write-mode))

(defgmethod
 (web-socket-peer+send-text :class 'web-socket-peer :bind "send_text" :hash
  166001499)
 error (message string))

(defgmethod
 (web-socket-peer+was-string-packet :class 'web-socket-peer :bind
  "was_string_packet" :hash 36873697)
 bool)

(defgmethod
 (web-socket-peer+poll :class 'web-socket-peer :bind "poll" :hash 3218959716)
 :void)

(defgmethod
 (web-socket-peer+close :class 'web-socket-peer :bind "close" :hash 1047156615)
 :void (code int) (reason string))

(defgmethod
 (web-socket-peer+get-connected-host :class 'web-socket-peer :bind
  "get_connected_host" :hash 201670096)
 string)

(defgmethod
 (web-socket-peer+get-connected-port :class 'web-socket-peer :bind
  "get_connected_port" :hash 3905245786)
 int)

(defgmethod
 (web-socket-peer+get-selected-protocol :class 'web-socket-peer :bind
  "get_selected_protocol" :hash 201670096)
 string)

(defgmethod
 (web-socket-peer+get-requested-url :class 'web-socket-peer :bind
  "get_requested_url" :hash 201670096)
 string)

(defgmethod
 (web-socket-peer+set-no-delay :class 'web-socket-peer :bind "set_no_delay"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (web-socket-peer+get-current-outbound-buffered-amount :class 'web-socket-peer
  :bind "get_current_outbound_buffered_amount" :hash 3905245786)
 int)

(defgmethod
 (web-socket-peer+get-ready-state :class 'web-socket-peer :bind
  "get_ready_state" :hash 346482985)
 web-socket-peer+state)

(defgmethod
 (web-socket-peer+get-close-code :class 'web-socket-peer :bind "get_close_code"
  :hash 3905245786)
 int)

(defgmethod
 (web-socket-peer+get-close-reason :class 'web-socket-peer :bind
  "get_close_reason" :hash 201670096)
 string)

(defgmethod
 (web-socket-peer+get-supported-protocols :class 'web-socket-peer :bind
  "get_supported_protocols" :hash 1139954409)
 packed-string-array)

(defgmethod
 (web-socket-peer+set-supported-protocols :class 'web-socket-peer :bind
  "set_supported_protocols" :hash 4015028928)
 :void (protocols packed-string-array))

(defgmethod
 (web-socket-peer+get-handshake-headers :class 'web-socket-peer :bind
  "get_handshake_headers" :hash 1139954409)
 packed-string-array)

(defgmethod
 (web-socket-peer+set-handshake-headers :class 'web-socket-peer :bind
  "set_handshake_headers" :hash 4015028928)
 :void (protocols packed-string-array))

(defgmethod
 (web-socket-peer+get-inbound-buffer-size :class 'web-socket-peer :bind
  "get_inbound_buffer_size" :hash 3905245786)
 int)

(defgmethod
 (web-socket-peer+set-inbound-buffer-size :class 'web-socket-peer :bind
  "set_inbound_buffer_size" :hash 1286410249)
 :void (buffer-size int))

(defgmethod
 (web-socket-peer+get-outbound-buffer-size :class 'web-socket-peer :bind
  "get_outbound_buffer_size" :hash 3905245786)
 int)

(defgmethod
 (web-socket-peer+set-outbound-buffer-size :class 'web-socket-peer :bind
  "set_outbound_buffer_size" :hash 1286410249)
 :void (buffer-size int))

(defgmethod
 (web-socket-peer+set-max-queued-packets :class 'web-socket-peer :bind
  "set_max_queued_packets" :hash 1286410249)
 :void (buffer-size int))

(defgmethod
 (web-socket-peer+get-max-queued-packets :class 'web-socket-peer :bind
  "get_max_queued_packets" :hash 3905245786)
 int)

(defgmethod
 (web-socket-peer+set-heartbeat-interval :class 'web-socket-peer :bind
  "set_heartbeat_interval" :hash 373806689)
 :void (interval float))

(defgmethod
 (web-socket-peer+get-heartbeat-interval :class 'web-socket-peer :bind
  "get_heartbeat_interval" :hash 1740695150)
 float)