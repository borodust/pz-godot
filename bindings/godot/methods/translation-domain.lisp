(common-lisp:in-package :%godot)


(defgmethod
 (translation-domain+get-translation-object :class 'translation-domain :bind
  "get_translation_object" :hash 606768082)
 translation (locale string))

(defgmethod
 (translation-domain+add-translation :class 'translation-domain :bind
  "add_translation" :hash 1466479800)
 :void (translation translation))

(defgmethod
 (translation-domain+remove-translation :class 'translation-domain :bind
  "remove_translation" :hash 1466479800)
 :void (translation translation))

(defgmethod
 (translation-domain+clear :class 'translation-domain :bind "clear" :hash
  3218959716)
 :void)

(defgmethod
 (translation-domain+get-translations :class 'translation-domain :bind
  "get_translations" :hash 3995934104)
 array)

(defgmethod
 (translation-domain+has-translation-for-locale :class 'translation-domain
  :bind "has_translation_for_locale" :hash 2034713381)
 bool (locale string) (exact bool))

(defgmethod
 (translation-domain+has-translation :class 'translation-domain :bind
  "has_translation" :hash 2696976312)
 bool (translation translation))

(defgmethod
 (translation-domain+find-translations :class 'translation-domain :bind
  "find_translations" :hash 2109650934)
 array (locale string) (exact bool))

(defgmethod
 (translation-domain+translate :class 'translation-domain :bind "translate"
  :hash 1829228469)
 string-name (message string-name) (context string-name))

(defgmethod
 (translation-domain+translate-plural :class 'translation-domain :bind
  "translate_plural" :hash 229954002)
 string-name (message string-name) (message-plural string-name) (n int)
 (context string-name))

(defgmethod
 (translation-domain+get-locale-override :class 'translation-domain :bind
  "get_locale_override" :hash 201670096)
 string)

(defgmethod
 (translation-domain+set-locale-override :class 'translation-domain :bind
  "set_locale_override" :hash 83702148)
 :void (locale string))

(defgmethod
 (translation-domain+is-enabled :class 'translation-domain :bind "is_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (translation-domain+set-enabled :class 'translation-domain :bind "set_enabled"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (translation-domain+is-pseudolocalization-enabled :class 'translation-domain
  :bind "is_pseudolocalization_enabled" :hash 36873697)
 bool)

(defgmethod
 (translation-domain+set-pseudolocalization-enabled :class 'translation-domain
  :bind "set_pseudolocalization_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (translation-domain+is-pseudolocalization-accents-enabled :class
  'translation-domain :bind "is_pseudolocalization_accents_enabled" :hash
  36873697)
 bool)

(defgmethod
 (translation-domain+set-pseudolocalization-accents-enabled :class
  'translation-domain :bind "set_pseudolocalization_accents_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (translation-domain+is-pseudolocalization-double-vowels-enabled :class
  'translation-domain :bind "is_pseudolocalization_double_vowels_enabled" :hash
  36873697)
 bool)

(defgmethod
 (translation-domain+set-pseudolocalization-double-vowels-enabled :class
  'translation-domain :bind "set_pseudolocalization_double_vowels_enabled"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (translation-domain+is-pseudolocalization-fake-bidi-enabled :class
  'translation-domain :bind "is_pseudolocalization_fake_bidi_enabled" :hash
  36873697)
 bool)

(defgmethod
 (translation-domain+set-pseudolocalization-fake-bidi-enabled :class
  'translation-domain :bind "set_pseudolocalization_fake_bidi_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (translation-domain+is-pseudolocalization-override-enabled :class
  'translation-domain :bind "is_pseudolocalization_override_enabled" :hash
  36873697)
 bool)

(defgmethod
 (translation-domain+set-pseudolocalization-override-enabled :class
  'translation-domain :bind "set_pseudolocalization_override_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (translation-domain+is-pseudolocalization-skip-placeholders-enabled :class
  'translation-domain :bind "is_pseudolocalization_skip_placeholders_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (translation-domain+set-pseudolocalization-skip-placeholders-enabled :class
  'translation-domain :bind "set_pseudolocalization_skip_placeholders_enabled"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (translation-domain+get-pseudolocalization-expansion-ratio :class
  'translation-domain :bind "get_pseudolocalization_expansion_ratio" :hash
  1740695150)
 float)

(defgmethod
 (translation-domain+set-pseudolocalization-expansion-ratio :class
  'translation-domain :bind "set_pseudolocalization_expansion_ratio" :hash
  373806689)
 :void (ratio float))

(defgmethod
 (translation-domain+get-pseudolocalization-prefix :class 'translation-domain
  :bind "get_pseudolocalization_prefix" :hash 201670096)
 string)

(defgmethod
 (translation-domain+set-pseudolocalization-prefix :class 'translation-domain
  :bind "set_pseudolocalization_prefix" :hash 83702148)
 :void (prefix string))

(defgmethod
 (translation-domain+get-pseudolocalization-suffix :class 'translation-domain
  :bind "get_pseudolocalization_suffix" :hash 201670096)
 string)

(defgmethod
 (translation-domain+set-pseudolocalization-suffix :class 'translation-domain
  :bind "set_pseudolocalization_suffix" :hash 83702148)
 :void (suffix string))

(defgmethod
 (translation-domain+pseudolocalize :class 'translation-domain :bind
  "pseudolocalize" :hash 1965194235)
 string-name (message string-name))