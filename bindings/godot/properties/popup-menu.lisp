(common-lisp:in-package :%godot)


(defgproperty popup-menu+hide-on-item-selection 'popup-menu :get
 'popup-menu+is-hide-on-item-selection :set
 'popup-menu+set-hide-on-item-selection)

(defgproperty popup-menu+hide-on-checkable-item-selection 'popup-menu :get
 'popup-menu+is-hide-on-checkable-item-selection :set
 'popup-menu+set-hide-on-checkable-item-selection)

(defgproperty popup-menu+hide-on-state-item-selection 'popup-menu :get
 'popup-menu+is-hide-on-state-item-selection :set
 'popup-menu+set-hide-on-state-item-selection)

(defgproperty popup-menu+submenu-popup-delay 'popup-menu :get
 'popup-menu+get-submenu-popup-delay :set 'popup-menu+set-submenu-popup-delay)

(defgproperty popup-menu+allow-search 'popup-menu :get
 'popup-menu+get-allow-search :set 'popup-menu+set-allow-search)

(defgproperty popup-menu+system-menu-id 'popup-menu :get
 'popup-menu+get-system-menu :set 'popup-menu+set-system-menu)

(defgproperty popup-menu+prefer-native-menu 'popup-menu :get
 'popup-menu+is-prefer-native-menu :set 'popup-menu+set-prefer-native-menu)

(defgproperty popup-menu+shrink-height 'popup-menu :get
 'popup-menu+get-shrink-height :set 'popup-menu+set-shrink-height)

(defgproperty popup-menu+shrink-width 'popup-menu :get
 'popup-menu+get-shrink-width :set 'popup-menu+set-shrink-width)

(defgproperty popup-menu+search-bar-enabled 'popup-menu :get
 'popup-menu+is-search-bar-enabled :set 'popup-menu+set-search-bar-enabled)

(defgproperty popup-menu+search-bar-min-item-count 'popup-menu :get
 'popup-menu+get-search-bar-min-item-count :set
 'popup-menu+set-search-bar-min-item-count)

(defgproperty popup-menu+search-bar-fuzzy-search-enabled 'popup-menu :get
 'popup-menu+is-search-bar-fuzzy-search-enabled :set
 'popup-menu+set-search-bar-fuzzy-search-enabled)

(defgproperty popup-menu+search-bar-fuzzy-search-max-misses 'popup-menu :get
 'popup-menu+get-search-bar-fuzzy-search-max-misses :set
 'popup-menu+set-search-bar-fuzzy-search-max-misses)

(defgproperty popup-menu+item-count 'popup-menu :get 'popup-menu+get-item-count
 :set 'popup-menu+set-item-count)