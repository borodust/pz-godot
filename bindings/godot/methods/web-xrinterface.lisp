(common-lisp:in-package :%godot)


(defgmethod
 (web-xrinterface+is-session-supported :class 'web-xrinterface :bind
  "is_session_supported" :hash 83702148)
 :void (session-mode string))

(defgmethod
 (web-xrinterface+set-session-mode :class 'web-xrinterface :bind
  "set_session_mode" :hash 83702148)
 :void (session-mode string))

(defgmethod
 (web-xrinterface+get-session-mode :class 'web-xrinterface :bind
  "get_session_mode" :hash 201670096)
 string)

(defgmethod
 (web-xrinterface+set-required-features :class 'web-xrinterface :bind
  "set_required_features" :hash 83702148)
 :void (required-features string))

(defgmethod
 (web-xrinterface+get-required-features :class 'web-xrinterface :bind
  "get_required_features" :hash 201670096)
 string)

(defgmethod
 (web-xrinterface+set-optional-features :class 'web-xrinterface :bind
  "set_optional_features" :hash 83702148)
 :void (optional-features string))

(defgmethod
 (web-xrinterface+get-optional-features :class 'web-xrinterface :bind
  "get_optional_features" :hash 201670096)
 string)

(defgmethod
 (web-xrinterface+get-reference-space-type :class 'web-xrinterface :bind
  "get_reference_space_type" :hash 201670096)
 string)

(defgmethod
 (web-xrinterface+get-enabled-features :class 'web-xrinterface :bind
  "get_enabled_features" :hash 201670096)
 string)

(defgmethod
 (web-xrinterface+set-requested-reference-space-types :class 'web-xrinterface
  :bind "set_requested_reference_space_types" :hash 83702148)
 :void (requested-reference-space-types string))

(defgmethod
 (web-xrinterface+get-requested-reference-space-types :class 'web-xrinterface
  :bind "get_requested_reference_space_types" :hash 201670096)
 string)

(defgmethod
 (web-xrinterface+is-input-source-active :class 'web-xrinterface :bind
  "is_input_source_active" :hash 1116898809)
 bool (input-source-id int))

(defgmethod
 (web-xrinterface+get-input-source-tracker :class 'web-xrinterface :bind
  "get_input_source_tracker" :hash 399776966)
 xrcontroller-tracker (input-source-id int))

(defgmethod
 (web-xrinterface+get-input-source-target-ray-mode :class 'web-xrinterface
  :bind "get_input_source_target_ray_mode" :hash 2852387453)
 web-xrinterface+target-ray-mode (input-source-id int))

(defgmethod
 (web-xrinterface+get-visibility-state :class 'web-xrinterface :bind
  "get_visibility_state" :hash 201670096)
 string)

(defgmethod
 (web-xrinterface+get-display-refresh-rate :class 'web-xrinterface :bind
  "get_display_refresh_rate" :hash 1740695150)
 float)

(defgmethod
 (web-xrinterface+set-display-refresh-rate :class 'web-xrinterface :bind
  "set_display_refresh_rate" :hash 373806689)
 :void (refresh-rate float))

(defgmethod
 (web-xrinterface+get-available-display-refresh-rates :class 'web-xrinterface
  :bind "get_available_display_refresh_rates" :hash 3995934104)
 array)