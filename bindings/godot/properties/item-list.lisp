(common-lisp:in-package :%godot)


(defgproperty item-list+select-mode 'item-list :get 'item-list+get-select-mode
 :set 'item-list+set-select-mode)

(defgproperty item-list+allow-reselect 'item-list :get
 'item-list+get-allow-reselect :set 'item-list+set-allow-reselect)

(defgproperty item-list+allow-rmb-select 'item-list :get
 'item-list+get-allow-rmb-select :set 'item-list+set-allow-rmb-select)

(defgproperty item-list+allow-search 'item-list :get
 'item-list+get-allow-search :set 'item-list+set-allow-search)

(defgproperty item-list+max-text-lines 'item-list :get
 'item-list+get-max-text-lines :set 'item-list+set-max-text-lines)

(defgproperty item-list+auto-width 'item-list :get 'item-list+has-auto-width
 :set 'item-list+set-auto-width)

(defgproperty item-list+auto-height 'item-list :get 'item-list+has-auto-height
 :set 'item-list+set-auto-height)

(defgproperty item-list+text-overrun-behavior 'item-list :get
 'item-list+get-text-overrun-behavior :set 'item-list+set-text-overrun-behavior)

(defgproperty item-list+wraparound-items 'item-list :get
 'item-list+has-wraparound-items :set 'item-list+set-wraparound-items)

(defgproperty item-list+scroll-hint-mode 'item-list :get
 'item-list+get-scroll-hint-mode :set 'item-list+set-scroll-hint-mode)

(defgproperty item-list+tile-scroll-hint 'item-list :get
 'item-list+is-scroll-hint-tiled :set 'item-list+set-tile-scroll-hint)

(defgproperty item-list+item-count 'item-list :get 'item-list+get-item-count
 :set 'item-list+set-item-count)

(defgproperty item-list+max-columns 'item-list :get 'item-list+get-max-columns
 :set 'item-list+set-max-columns)

(defgproperty item-list+same-column-width 'item-list :get
 'item-list+is-same-column-width :set 'item-list+set-same-column-width)

(defgproperty item-list+fixed-column-width 'item-list :get
 'item-list+get-fixed-column-width :set 'item-list+set-fixed-column-width)

(defgproperty item-list+icon-mode 'item-list :get 'item-list+get-icon-mode :set
 'item-list+set-icon-mode)

(defgproperty item-list+icon-scale 'item-list :get 'item-list+get-icon-scale
 :set 'item-list+set-icon-scale)

(defgproperty item-list+fixed-icon-size 'item-list :get
 'item-list+get-fixed-icon-size :set 'item-list+set-fixed-icon-size)