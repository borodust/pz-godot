(common-lisp:in-package :%godot)


(defgproperty code-edit+symbol-lookup-on-click 'code-edit :get
 'code-edit+is-symbol-lookup-on-click-enabled :set
 'code-edit+set-symbol-lookup-on-click-enabled)

(defgproperty code-edit+symbol-tooltip-on-hover 'code-edit :get
 'code-edit+is-symbol-tooltip-on-hover-enabled :set
 'code-edit+set-symbol-tooltip-on-hover-enabled)

(defgproperty code-edit+line-folding 'code-edit :get
 'code-edit+is-line-folding-enabled :set 'code-edit+set-line-folding-enabled)

(defgproperty code-edit+line-length-guidelines 'code-edit :get
 'code-edit+get-line-length-guidelines :set
 'code-edit+set-line-length-guidelines)

(defgproperty code-edit+gutters-draw-breakpoints-gutter 'code-edit :get
 'code-edit+is-drawing-breakpoints-gutter :set
 'code-edit+set-draw-breakpoints-gutter)

(defgproperty code-edit+gutters-draw-bookmarks 'code-edit :get
 'code-edit+is-drawing-bookmarks-gutter :set
 'code-edit+set-draw-bookmarks-gutter)

(defgproperty code-edit+gutters-draw-executing-lines 'code-edit :get
 'code-edit+is-drawing-executing-lines-gutter :set
 'code-edit+set-draw-executing-lines-gutter)

(defgproperty code-edit+gutters-draw-line-numbers 'code-edit :get
 'code-edit+is-draw-line-numbers-enabled :set 'code-edit+set-draw-line-numbers)

(defgproperty code-edit+gutters-zero-pad-line-numbers 'code-edit :get
 'code-edit+is-line-numbers-zero-padded :set
 'code-edit+set-line-numbers-zero-padded)

(defgproperty code-edit+gutters-line-numbers-min-digits 'code-edit :get
 'code-edit+get-line-numbers-min-digits :set
 'code-edit+set-line-numbers-min-digits)

(defgproperty code-edit+gutters-draw-fold-gutter 'code-edit :get
 'code-edit+is-drawing-fold-gutter :set 'code-edit+set-draw-fold-gutter)

(defgproperty code-edit+delimiter-strings 'code-edit :get
 'code-edit+get-string-delimiters :set 'code-edit+set-string-delimiters)

(defgproperty code-edit+delimiter-comments 'code-edit :get
 'code-edit+get-comment-delimiters :set 'code-edit+set-comment-delimiters)

(defgproperty code-edit+code-completion-enabled 'code-edit :get
 'code-edit+is-code-completion-enabled :set
 'code-edit+set-code-completion-enabled)

(defgproperty code-edit+code-completion-prefixes 'code-edit :get
 'code-edit+get-code-completion-prefixes :set
 'code-edit+set-code-completion-prefixes)

(defgproperty code-edit+indent-size 'code-edit :get 'code-edit+get-indent-size
 :set 'code-edit+set-indent-size)

(defgproperty code-edit+indent-use-spaces 'code-edit :get
 'code-edit+is-indent-using-spaces :set 'code-edit+set-indent-using-spaces)

(defgproperty code-edit+indent-automatic 'code-edit :get
 'code-edit+is-auto-indent-enabled :set 'code-edit+set-auto-indent-enabled)

(defgproperty code-edit+indent-automatic-prefixes 'code-edit :get
 'code-edit+get-auto-indent-prefixes :set 'code-edit+set-auto-indent-prefixes)

(defgproperty code-edit+auto-brace-completion-enabled 'code-edit :get
 'code-edit+is-auto-brace-completion-enabled :set
 'code-edit+set-auto-brace-completion-enabled)

(defgproperty code-edit+auto-brace-completion-highlight-matching 'code-edit
 :get 'code-edit+is-highlight-matching-braces-enabled :set
 'code-edit+set-highlight-matching-braces-enabled)

(defgproperty code-edit+auto-brace-completion-pairs 'code-edit :get
 'code-edit+get-auto-brace-completion-pairs :set
 'code-edit+set-auto-brace-completion-pairs)