(common-lisp:in-package :%godot)


(defgproperty tree+columns 'tree :get 'tree+get-columns :set 'tree+set-columns)

(defgproperty tree+column-titles-visible 'tree :get
 'tree+are-column-titles-visible :set 'tree+set-column-titles-visible)

(defgproperty tree+allow-reselect 'tree :get 'tree+get-allow-reselect :set
 'tree+set-allow-reselect)

(defgproperty tree+allow-rmb-select 'tree :get 'tree+get-allow-rmb-select :set
 'tree+set-allow-rmb-select)

(defgproperty tree+allow-search 'tree :get 'tree+get-allow-search :set
 'tree+set-allow-search)

(defgproperty tree+hide-folding 'tree :get 'tree+is-folding-hidden :set
 'tree+set-hide-folding)

(defgproperty tree+enable-recursive-folding 'tree :get
 'tree+is-recursive-folding-enabled :set 'tree+set-enable-recursive-folding)

(defgproperty tree+enable-drag-unfolding 'tree :get
 'tree+is-drag-unfolding-enabled :set 'tree+set-enable-drag-unfolding)

(defgproperty tree+hide-root 'tree :get 'tree+is-root-hidden :set
 'tree+set-hide-root)

(defgproperty tree+drop-mode-flags 'tree :get 'tree+get-drop-mode-flags :set
 'tree+set-drop-mode-flags)

(defgproperty tree+select-mode 'tree :get 'tree+get-select-mode :set
 'tree+set-select-mode)

(defgproperty tree+auto-tooltip 'tree :get 'tree+is-auto-tooltip-enabled :set
 'tree+set-auto-tooltip)

(defgproperty tree+scroll-horizontal-enabled 'tree :get
 'tree+is-h-scroll-enabled :set 'tree+set-h-scroll-enabled)

(defgproperty tree+scroll-vertical-enabled 'tree :get 'tree+is-v-scroll-enabled
 :set 'tree+set-v-scroll-enabled)

(defgproperty tree+scroll-hint-mode 'tree :get 'tree+get-scroll-hint-mode :set
 'tree+set-scroll-hint-mode)

(defgproperty tree+tile-scroll-hint 'tree :get 'tree+is-scroll-hint-tiled :set
 'tree+set-tile-scroll-hint)