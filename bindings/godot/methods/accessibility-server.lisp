(common-lisp:in-package :%godot)


(defgmethod
 (accessibility-server+is-supported :class 'accessibility-server :bind
  "is_supported" :hash 36873697)
 bool)

(defgmethod
 (accessibility-server+create-element :class 'accessibility-server :bind
  "create_element" :hash 3846965249)
 rid (window-id int) (role accessibility-server+accessibility-role))

(defgmethod
 (accessibility-server+create-sub-element :class 'accessibility-server :bind
  "create_sub_element" :hash 1151690429)
 rid (parent-rid rid) (role accessibility-server+accessibility-role)
 (insert-pos int))

(defgmethod
 (accessibility-server+create-sub-text-edit-elements :class
  'accessibility-server :bind "create_sub_text_edit_elements" :hash 2702009895)
 rid (parent-rid rid) (shaped-text rid) (min-height float) (insert-pos int)
 (is-last-line bool))

(defgmethod
 (accessibility-server+has-element :class 'accessibility-server :bind
  "has_element" :hash 4155700596)
 bool (id rid))

(defgmethod
 (accessibility-server+free-element :class 'accessibility-server :bind
  "free_element" :hash 2722037293)
 :void (id rid))

(defgmethod
 (accessibility-server+element-set-meta :class 'accessibility-server :bind
  "element_set_meta" :hash 3175752987)
 :void (id rid) (meta variant))

(defgmethod
 (accessibility-server+element-get-meta :class 'accessibility-server :bind
  "element_get_meta" :hash 4171304767)
 variant (id rid))

(defgmethod
 (accessibility-server+set-window-rect :class 'accessibility-server :bind
  "set_window_rect" :hash 2386961724)
 :void (window-id int) (rect-out rect-2) (rect-in rect-2))

(defgmethod
 (accessibility-server+set-window-focused :class 'accessibility-server :bind
  "set_window_focused" :hash 300928843)
 :void (window-id int) (focused bool))

(defgmethod
 (accessibility-server+update-set-focus :class 'accessibility-server :bind
  "update_set_focus" :hash 2722037293)
 :void (id rid))

(defgmethod
 (accessibility-server+get-window-root :class 'accessibility-server :bind
  "get_window_root" :hash 495598643)
 rid (window-id int))

(defgmethod
 (accessibility-server+update-set-role :class 'accessibility-server :bind
  "update_set_role" :hash 3747886520)
 :void (id rid) (role accessibility-server+accessibility-role))

(defgmethod
 (accessibility-server+update-set-name :class 'accessibility-server :bind
  "update_set_name" :hash 2726140452)
 :void (id rid) (name string))

(defgmethod
 (accessibility-server+update-set-braille-label :class 'accessibility-server
  :bind "update_set_braille_label" :hash 2726140452)
 :void (id rid) (name string))

(defgmethod
 (accessibility-server+update-set-braille-role-description :class
  'accessibility-server :bind "update_set_braille_role_description" :hash
  2726140452)
 :void (id rid) (description string))

(defgmethod
 (accessibility-server+update-set-extra-info :class 'accessibility-server :bind
  "update_set_extra_info" :hash 2726140452)
 :void (id rid) (name string))

(defgmethod
 (accessibility-server+update-set-description :class 'accessibility-server
  :bind "update_set_description" :hash 2726140452)
 :void (id rid) (description string))

(defgmethod
 (accessibility-server+update-set-value :class 'accessibility-server :bind
  "update_set_value" :hash 2726140452)
 :void (id rid) (value string))

(defgmethod
 (accessibility-server+update-set-tooltip :class 'accessibility-server :bind
  "update_set_tooltip" :hash 2726140452)
 :void (id rid) (tooltip string))

(defgmethod
 (accessibility-server+update-set-bounds :class 'accessibility-server :bind
  "update_set_bounds" :hash 1378122625)
 :void (id rid) (rect rect-2))

(defgmethod
 (accessibility-server+update-set-transform :class 'accessibility-server :bind
  "update_set_transform" :hash 1246044741)
 :void (id rid) (transform transform-2d))

(defgmethod
 (accessibility-server+update-add-child :class 'accessibility-server :bind
  "update_add_child" :hash 395945892)
 :void (id rid) (child-id rid))

(defgmethod
 (accessibility-server+update-add-related-controls :class 'accessibility-server
  :bind "update_add_related_controls" :hash 395945892)
 :void (id rid) (related-id rid))

(defgmethod
 (accessibility-server+update-add-related-details :class 'accessibility-server
  :bind "update_add_related_details" :hash 395945892)
 :void (id rid) (related-id rid))

(defgmethod
 (accessibility-server+update-add-related-described-by :class
  'accessibility-server :bind "update_add_related_described_by" :hash
  395945892)
 :void (id rid) (related-id rid))

(defgmethod
 (accessibility-server+update-add-related-flow-to :class 'accessibility-server
  :bind "update_add_related_flow_to" :hash 395945892)
 :void (id rid) (related-id rid))

(defgmethod
 (accessibility-server+update-add-related-labeled-by :class
  'accessibility-server :bind "update_add_related_labeled_by" :hash 395945892)
 :void (id rid) (related-id rid))

(defgmethod
 (accessibility-server+update-add-related-radio-group :class
  'accessibility-server :bind "update_add_related_radio_group" :hash 395945892)
 :void (id rid) (related-id rid))

(defgmethod
 (accessibility-server+update-set-active-descendant :class
  'accessibility-server :bind "update_set_active_descendant" :hash 395945892)
 :void (id rid) (other-id rid))

(defgmethod
 (accessibility-server+update-set-next-on-line :class 'accessibility-server
  :bind "update_set_next_on_line" :hash 395945892)
 :void (id rid) (other-id rid))

(defgmethod
 (accessibility-server+update-set-previous-on-line :class 'accessibility-server
  :bind "update_set_previous_on_line" :hash 395945892)
 :void (id rid) (other-id rid))

(defgmethod
 (accessibility-server+update-set-member-of :class 'accessibility-server :bind
  "update_set_member_of" :hash 395945892)
 :void (id rid) (group-id rid))

(defgmethod
 (accessibility-server+update-set-in-page-link-target :class
  'accessibility-server :bind "update_set_in_page_link_target" :hash 395945892)
 :void (id rid) (other-id rid))

(defgmethod
 (accessibility-server+update-set-error-message :class 'accessibility-server
  :bind "update_set_error_message" :hash 395945892)
 :void (id rid) (other-id rid))

(defgmethod
 (accessibility-server+update-set-live :class 'accessibility-server :bind
  "update_set_live" :hash 2993365237)
 :void (id rid) (live accessibility-server+accessibility-live-mode))

(defgmethod
 (accessibility-server+update-add-action :class 'accessibility-server :bind
  "update_add_action" :hash 3960092835)
 :void (id rid) (action accessibility-server+accessibility-action)
 (callable callable))

(defgmethod
 (accessibility-server+update-add-custom-action :class 'accessibility-server
  :bind "update_add_custom_action" :hash 4153150897)
 :void (id rid) (action-id int) (action-description string))

(defgmethod
 (accessibility-server+update-set-table-row-count :class 'accessibility-server
  :bind "update_set_table_row_count" :hash 3411492887)
 :void (id rid) (count int))

(defgmethod
 (accessibility-server+update-set-table-column-count :class
  'accessibility-server :bind "update_set_table_column_count" :hash 3411492887)
 :void (id rid) (count int))

(defgmethod
 (accessibility-server+update-set-table-row-index :class 'accessibility-server
  :bind "update_set_table_row_index" :hash 3411492887)
 :void (id rid) (index int))

(defgmethod
 (accessibility-server+update-set-table-column-index :class
  'accessibility-server :bind "update_set_table_column_index" :hash 3411492887)
 :void (id rid) (index int))

(defgmethod
 (accessibility-server+update-set-table-cell-position :class
  'accessibility-server :bind "update_set_table_cell_position" :hash
  4288446313)
 :void (id rid) (row-index int) (column-index int))

(defgmethod
 (accessibility-server+update-set-table-cell-span :class 'accessibility-server
  :bind "update_set_table_cell_span" :hash 4288446313)
 :void (id rid) (row-span int) (column-span int))

(defgmethod
 (accessibility-server+update-set-list-item-count :class 'accessibility-server
  :bind "update_set_list_item_count" :hash 3411492887)
 :void (id rid) (size int))

(defgmethod
 (accessibility-server+update-set-list-item-index :class 'accessibility-server
  :bind "update_set_list_item_index" :hash 3411492887)
 :void (id rid) (index int))

(defgmethod
 (accessibility-server+update-set-list-item-level :class 'accessibility-server
  :bind "update_set_list_item_level" :hash 3411492887)
 :void (id rid) (level int))

(defgmethod
 (accessibility-server+update-set-list-item-selected :class
  'accessibility-server :bind "update_set_list_item_selected" :hash 1265174801)
 :void (id rid) (selected bool))

(defgmethod
 (accessibility-server+update-set-list-item-expanded :class
  'accessibility-server :bind "update_set_list_item_expanded" :hash 1265174801)
 :void (id rid) (expanded bool))

(defgmethod
 (accessibility-server+update-set-popup-type :class 'accessibility-server :bind
  "update_set_popup_type" :hash 690307634)
 :void (id rid) (popup accessibility-server+accessibility-popup-type))

(defgmethod
 (accessibility-server+update-set-checked :class 'accessibility-server :bind
  "update_set_checked" :hash 1265174801)
 :void (id rid) (checekd bool))

(defgmethod
 (accessibility-server+update-set-num-value :class 'accessibility-server :bind
  "update_set_num_value" :hash 1794382983)
 :void (id rid) (position float))

(defgmethod
 (accessibility-server+update-set-num-range :class 'accessibility-server :bind
  "update_set_num_range" :hash 2513314492)
 :void (id rid) (min float) (max float))

(defgmethod
 (accessibility-server+update-set-num-step :class 'accessibility-server :bind
  "update_set_num_step" :hash 1794382983)
 :void (id rid) (step float))

(defgmethod
 (accessibility-server+update-set-num-jump :class 'accessibility-server :bind
  "update_set_num_jump" :hash 1794382983)
 :void (id rid) (jump float))

(defgmethod
 (accessibility-server+update-set-scroll-x :class 'accessibility-server :bind
  "update_set_scroll_x" :hash 1794382983)
 :void (id rid) (position float))

(defgmethod
 (accessibility-server+update-set-scroll-x-range :class 'accessibility-server
  :bind "update_set_scroll_x_range" :hash 2513314492)
 :void (id rid) (min float) (max float))

(defgmethod
 (accessibility-server+update-set-scroll-y :class 'accessibility-server :bind
  "update_set_scroll_y" :hash 1794382983)
 :void (id rid) (position float))

(defgmethod
 (accessibility-server+update-set-scroll-y-range :class 'accessibility-server
  :bind "update_set_scroll_y_range" :hash 2513314492)
 :void (id rid) (min float) (max float))

(defgmethod
 (accessibility-server+update-set-text-decorations :class 'accessibility-server
  :bind "update_set_text_decorations" :hash 457503484)
 :void (id rid) (underline bool) (strikethrough bool) (overline bool)
 (color color))

(defgmethod
 (accessibility-server+update-set-text-align :class 'accessibility-server :bind
  "update_set_text_align" :hash 3725995085)
 :void (id rid) (align horizontal-alignment))

(defgmethod
 (accessibility-server+update-set-text-selection :class 'accessibility-server
  :bind "update_set_text_selection" :hash 3119144029)
 :void (id rid) (text-start-id rid) (start-char int) (text-end-id rid)
 (end-char int))

(defgmethod
 (accessibility-server+update-set-flag :class 'accessibility-server :bind
  "update_set_flag" :hash 1473043386)
 :void (id rid) (flag accessibility-server+accessibility-flags) (value bool))

(defgmethod
 (accessibility-server+update-set-classname :class 'accessibility-server :bind
  "update_set_classname" :hash 2726140452)
 :void (id rid) (classname string))

(defgmethod
 (accessibility-server+update-set-placeholder :class 'accessibility-server
  :bind "update_set_placeholder" :hash 2726140452)
 :void (id rid) (placeholder string))

(defgmethod
 (accessibility-server+update-set-language :class 'accessibility-server :bind
  "update_set_language" :hash 2726140452)
 :void (id rid) (language string))

(defgmethod
 (accessibility-server+update-set-text-orientation :class 'accessibility-server
  :bind "update_set_text_orientation" :hash 1265174801)
 :void (id rid) (vertical bool))

(defgmethod
 (accessibility-server+update-set-list-orientation :class 'accessibility-server
  :bind "update_set_list_orientation" :hash 1265174801)
 :void (id rid) (vertical bool))

(defgmethod
 (accessibility-server+update-set-shortcut :class 'accessibility-server :bind
  "update_set_shortcut" :hash 2726140452)
 :void (id rid) (shortcut string))

(defgmethod
 (accessibility-server+update-set-url :class 'accessibility-server :bind
  "update_set_url" :hash 2726140452)
 :void (id rid) (url string))

(defgmethod
 (accessibility-server+update-set-role-description :class 'accessibility-server
  :bind "update_set_role_description" :hash 2726140452)
 :void (id rid) (description string))

(defgmethod
 (accessibility-server+update-set-state-description :class
  'accessibility-server :bind "update_set_state_description" :hash 2726140452)
 :void (id rid) (description string))

(defgmethod
 (accessibility-server+update-set-color-value :class 'accessibility-server
  :bind "update_set_color_value" :hash 2948539648)
 :void (id rid) (color color))

(defgmethod
 (accessibility-server+update-set-background-color :class 'accessibility-server
  :bind "update_set_background_color" :hash 2948539648)
 :void (id rid) (color color))

(defgmethod
 (accessibility-server+update-set-foreground-color :class 'accessibility-server
  :bind "update_set_foreground_color" :hash 2948539648)
 :void (id rid) (color color))