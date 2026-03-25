(common-lisp:in-package :%godot)


(defgmethod
 (graph-node+-draw-port :class 'graph-node :bind "_draw_port" :hash 93366828
  :virtual common-lisp:t)
 :void (slot-index int) (position vector-2i) (left bool) (color color))

(defgmethod
 (graph-node+set-title :class 'graph-node :bind "set_title" :hash 83702148)
 :void (title string))

(defgmethod
 (graph-node+get-title :class 'graph-node :bind "get_title" :hash 201670096)
 string)

(defgmethod
 (graph-node+get-titlebar-hbox :class 'graph-node :bind "get_titlebar_hbox"
  :hash 3590609951)
 hbox-container)

(defgmethod
 (graph-node+set-slot :class 'graph-node :bind "set_slot" :hash 2873310869)
 :void (slot-index int) (enable-left-port bool) (type-left int)
 (color-left color) (enable-right-port bool) (type-right int)
 (color-right color) (custom-icon-left texture-2d)
 (custom-icon-right texture-2d) (draw-stylebox bool))

(defgmethod
 (graph-node+clear-slot :class 'graph-node :bind "clear_slot" :hash 1286410249)
 :void (slot-index int))

(defgmethod
 (graph-node+clear-all-slots :class 'graph-node :bind "clear_all_slots" :hash
  3218959716)
 :void)

(defgmethod
 (graph-node+is-slot-enabled-left :class 'graph-node :bind
  "is_slot_enabled_left" :hash 1116898809)
 bool (slot-index int))

(defgmethod
 (graph-node+set-slot-enabled-left :class 'graph-node :bind
  "set_slot_enabled_left" :hash 300928843)
 :void (slot-index int) (enable bool))

(defgmethod
 (graph-node+set-slot-type-left :class 'graph-node :bind "set_slot_type_left"
  :hash 3937882851)
 :void (slot-index int) (type int))

(defgmethod
 (graph-node+get-slot-type-left :class 'graph-node :bind "get_slot_type_left"
  :hash 923996154)
 int (slot-index int))

(defgmethod
 (graph-node+set-slot-color-left :class 'graph-node :bind "set_slot_color_left"
  :hash 2878471219)
 :void (slot-index int) (color color))

(defgmethod
 (graph-node+get-slot-color-left :class 'graph-node :bind "get_slot_color_left"
  :hash 3457211756)
 color (slot-index int))

(defgmethod
 (graph-node+set-slot-custom-icon-left :class 'graph-node :bind
  "set_slot_custom_icon_left" :hash 666127730)
 :void (slot-index int) (custom-icon texture-2d))

(defgmethod
 (graph-node+get-slot-custom-icon-left :class 'graph-node :bind
  "get_slot_custom_icon_left" :hash 3536238170)
 texture-2d (slot-index int))

(defgmethod
 (graph-node+set-slot-metadata-left :class 'graph-node :bind
  "set_slot_metadata_left" :hash 2152698145)
 :void (slot-index int) (value variant))

(defgmethod
 (graph-node+get-slot-metadata-left :class 'graph-node :bind
  "get_slot_metadata_left" :hash 4227898402)
 variant (slot-index int))

(defgmethod
 (graph-node+is-slot-enabled-right :class 'graph-node :bind
  "is_slot_enabled_right" :hash 1116898809)
 bool (slot-index int))

(defgmethod
 (graph-node+set-slot-enabled-right :class 'graph-node :bind
  "set_slot_enabled_right" :hash 300928843)
 :void (slot-index int) (enable bool))

(defgmethod
 (graph-node+set-slot-type-right :class 'graph-node :bind "set_slot_type_right"
  :hash 3937882851)
 :void (slot-index int) (type int))

(defgmethod
 (graph-node+get-slot-type-right :class 'graph-node :bind "get_slot_type_right"
  :hash 923996154)
 int (slot-index int))

(defgmethod
 (graph-node+set-slot-color-right :class 'graph-node :bind
  "set_slot_color_right" :hash 2878471219)
 :void (slot-index int) (color color))

(defgmethod
 (graph-node+get-slot-color-right :class 'graph-node :bind
  "get_slot_color_right" :hash 3457211756)
 color (slot-index int))

(defgmethod
 (graph-node+set-slot-custom-icon-right :class 'graph-node :bind
  "set_slot_custom_icon_right" :hash 666127730)
 :void (slot-index int) (custom-icon texture-2d))

(defgmethod
 (graph-node+get-slot-custom-icon-right :class 'graph-node :bind
  "get_slot_custom_icon_right" :hash 3536238170)
 texture-2d (slot-index int))

(defgmethod
 (graph-node+set-slot-metadata-right :class 'graph-node :bind
  "set_slot_metadata_right" :hash 2152698145)
 :void (slot-index int) (value variant))

(defgmethod
 (graph-node+get-slot-metadata-right :class 'graph-node :bind
  "get_slot_metadata_right" :hash 4227898402)
 variant (slot-index int))

(defgmethod
 (graph-node+is-slot-draw-stylebox :class 'graph-node :bind
  "is_slot_draw_stylebox" :hash 1116898809)
 bool (slot-index int))

(defgmethod
 (graph-node+set-slot-draw-stylebox :class 'graph-node :bind
  "set_slot_draw_stylebox" :hash 300928843)
 :void (slot-index int) (enable bool))

(defgmethod
 (graph-node+set-ignore-invalid-connection-type :class 'graph-node :bind
  "set_ignore_invalid_connection_type" :hash 2586408642)
 :void (ignore bool))

(defgmethod
 (graph-node+is-ignoring-valid-connection-type :class 'graph-node :bind
  "is_ignoring_valid_connection_type" :hash 36873697)
 bool)

(defgmethod
 (graph-node+set-slots-focus-mode :class 'graph-node :bind
  "set_slots_focus_mode" :hash 3232914922)
 :void (focus-mode control+focus-mode))

(defgmethod
 (graph-node+get-slots-focus-mode :class 'graph-node :bind
  "get_slots_focus_mode" :hash 2132829277)
 control+focus-mode)

(defgmethod
 (graph-node+get-input-port-count :class 'graph-node :bind
  "get_input_port_count" :hash 2455072627)
 int)

(defgmethod
 (graph-node+get-input-port-position :class 'graph-node :bind
  "get_input_port_position" :hash 3114997196)
 vector-2 (port-idx int))

(defgmethod
 (graph-node+get-input-port-type :class 'graph-node :bind "get_input_port_type"
  :hash 3744713108)
 int (port-idx int))

(defgmethod
 (graph-node+get-input-port-color :class 'graph-node :bind
  "get_input_port_color" :hash 2624840992)
 color (port-idx int))

(defgmethod
 (graph-node+get-input-port-slot :class 'graph-node :bind "get_input_port_slot"
  :hash 3744713108)
 int (port-idx int))

(defgmethod
 (graph-node+get-output-port-count :class 'graph-node :bind
  "get_output_port_count" :hash 2455072627)
 int)

(defgmethod
 (graph-node+get-output-port-position :class 'graph-node :bind
  "get_output_port_position" :hash 3114997196)
 vector-2 (port-idx int))

(defgmethod
 (graph-node+get-output-port-type :class 'graph-node :bind
  "get_output_port_type" :hash 3744713108)
 int (port-idx int))

(defgmethod
 (graph-node+get-output-port-color :class 'graph-node :bind
  "get_output_port_color" :hash 2624840992)
 color (port-idx int))

(defgmethod
 (graph-node+get-output-port-slot :class 'graph-node :bind
  "get_output_port_slot" :hash 3744713108)
 int (port-idx int))