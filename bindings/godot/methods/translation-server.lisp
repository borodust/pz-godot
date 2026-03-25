(common-lisp:in-package :%godot)


(defgmethod
 (translation-server+set-locale :class 'translation-server :bind "set_locale"
  :hash 83702148)
 :void (locale string))

(defgmethod
 (translation-server+get-locale :class 'translation-server :bind "get_locale"
  :hash 201670096)
 string)

(defgmethod
 (translation-server+get-tool-locale :class 'translation-server :bind
  "get_tool_locale" :hash 2841200299)
 string)

(defgmethod
 (translation-server+compare-locales :class 'translation-server :bind
  "compare_locales" :hash 2878152881)
 int (locale-a string) (locale-b string))

(defgmethod
 (translation-server+standardize-locale :class 'translation-server :bind
  "standardize_locale" :hash 4216441673)
 string (locale string) (add-defaults bool))

(defgmethod
 (translation-server+get-all-languages :class 'translation-server :bind
  "get_all_languages" :hash 1139954409)
 packed-string-array)

(defgmethod
 (translation-server+get-language-name :class 'translation-server :bind
  "get_language_name" :hash 3135753539)
 string (language string))

(defgmethod
 (translation-server+get-all-scripts :class 'translation-server :bind
  "get_all_scripts" :hash 1139954409)
 packed-string-array)

(defgmethod
 (translation-server+get-script-name :class 'translation-server :bind
  "get_script_name" :hash 3135753539)
 string (script string))

(defgmethod
 (translation-server+get-all-countries :class 'translation-server :bind
  "get_all_countries" :hash 1139954409)
 packed-string-array)

(defgmethod
 (translation-server+get-country-name :class 'translation-server :bind
  "get_country_name" :hash 3135753539)
 string (country string))

(defgmethod
 (translation-server+get-locale-name :class 'translation-server :bind
  "get_locale_name" :hash 3135753539)
 string (locale string))

(defgmethod
 (translation-server+get-plural-rules :class 'translation-server :bind
  "get_plural_rules" :hash 3135753539)
 string (locale string))

(defgmethod
 (translation-server+translate :class 'translation-server :bind "translate"
  :hash 1829228469)
 string-name (message string-name) (context string-name))

(defgmethod
 (translation-server+translate-plural :class 'translation-server :bind
  "translate_plural" :hash 229954002)
 string-name (message string-name) (plural-message string-name) (n int)
 (context string-name))

(defgmethod
 (translation-server+add-translation :class 'translation-server :bind
  "add_translation" :hash 1466479800)
 :void (translation translation))

(defgmethod
 (translation-server+remove-translation :class 'translation-server :bind
  "remove_translation" :hash 1466479800)
 :void (translation translation))

(defgmethod
 (translation-server+get-translation-object :class 'translation-server :bind
  "get_translation_object" :hash 2065240175)
 translation (locale string))

(defgmethod
 (translation-server+get-translations :class 'translation-server :bind
  "get_translations" :hash 3995934104)
 array)

(defgmethod
 (translation-server+find-translations :class 'translation-server :bind
  "find_translations" :hash 2109650934)
 array (locale string) (exact bool))

(defgmethod
 (translation-server+has-translation-for-locale :class 'translation-server
  :bind "has_translation_for_locale" :hash 2034713381)
 bool (locale string) (exact bool))

(defgmethod
 (translation-server+has-translation :class 'translation-server :bind
  "has_translation" :hash 2696976312)
 bool (translation translation))

(defgmethod
 (translation-server+has-domain :class 'translation-server :bind "has_domain"
  :hash 2619796661)
 bool (domain string-name))

(defgmethod
 (translation-server+get-or-add-domain :class 'translation-server :bind
  "get_or_add_domain" :hash 397200075)
 translation-domain (domain string-name))

(defgmethod
 (translation-server+remove-domain :class 'translation-server :bind
  "remove_domain" :hash 3304788590)
 :void (domain string-name))

(defgmethod
 (translation-server+clear :class 'translation-server :bind "clear" :hash
  3218959716)
 :void)

(defgmethod
 (translation-server+get-loaded-locales :class 'translation-server :bind
  "get_loaded_locales" :hash 1139954409)
 packed-string-array)

(defgmethod
 (translation-server+format-number :class 'translation-server :bind
  "format_number" :hash 315676799)
 string (number string) (locale string))

(defgmethod
 (translation-server+get-percent-sign :class 'translation-server :bind
  "get_percent_sign" :hash 3135753539)
 string (locale string))

(defgmethod
 (translation-server+parse-number :class 'translation-server :bind
  "parse_number" :hash 315676799)
 string (number string) (locale string))

(defgmethod
 (translation-server+is-pseudolocalization-enabled :class 'translation-server
  :bind "is_pseudolocalization_enabled" :hash 36873697)
 bool)

(defgmethod
 (translation-server+set-pseudolocalization-enabled :class 'translation-server
  :bind "set_pseudolocalization_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (translation-server+reload-pseudolocalization :class 'translation-server :bind
  "reload_pseudolocalization" :hash 3218959716)
 :void)

(defgmethod
 (translation-server+pseudolocalize :class 'translation-server :bind
  "pseudolocalize" :hash 1965194235)
 string-name (message string-name))