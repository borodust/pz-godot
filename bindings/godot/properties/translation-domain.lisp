(common-lisp:in-package :%godot)


(defgproperty translation-domain+enabled 'translation-domain :get
 'translation-domain+is-enabled :set 'translation-domain+set-enabled)

(defgproperty translation-domain+pseudolocalization-enabled 'translation-domain
 :get 'translation-domain+is-pseudolocalization-enabled :set
 'translation-domain+set-pseudolocalization-enabled)

(defgproperty translation-domain+pseudolocalization-accents-enabled
 'translation-domain :get
 'translation-domain+is-pseudolocalization-accents-enabled :set
 'translation-domain+set-pseudolocalization-accents-enabled)

(defgproperty translation-domain+pseudolocalization-double-vowels-enabled
 'translation-domain :get
 'translation-domain+is-pseudolocalization-double-vowels-enabled :set
 'translation-domain+set-pseudolocalization-double-vowels-enabled)

(defgproperty translation-domain+pseudolocalization-fake-bidi-enabled
 'translation-domain :get
 'translation-domain+is-pseudolocalization-fake-bidi-enabled :set
 'translation-domain+set-pseudolocalization-fake-bidi-enabled)

(defgproperty translation-domain+pseudolocalization-override-enabled
 'translation-domain :get
 'translation-domain+is-pseudolocalization-override-enabled :set
 'translation-domain+set-pseudolocalization-override-enabled)

(defgproperty translation-domain+pseudolocalization-skip-placeholders-enabled
 'translation-domain :get
 'translation-domain+is-pseudolocalization-skip-placeholders-enabled :set
 'translation-domain+set-pseudolocalization-skip-placeholders-enabled)

(defgproperty translation-domain+pseudolocalization-expansion-ratio
 'translation-domain :get
 'translation-domain+get-pseudolocalization-expansion-ratio :set
 'translation-domain+set-pseudolocalization-expansion-ratio)

(defgproperty translation-domain+pseudolocalization-prefix 'translation-domain
 :get 'translation-domain+get-pseudolocalization-prefix :set
 'translation-domain+set-pseudolocalization-prefix)

(defgproperty translation-domain+pseudolocalization-suffix 'translation-domain
 :get 'translation-domain+get-pseudolocalization-suffix :set
 'translation-domain+set-pseudolocalization-suffix)