(common-lisp:in-package :%godot)


(defgmethod
 (gdscript-text-document+show-native-symbol-in-editor :class
  'gdscript-text-document :bind "show_native_symbol_in_editor" :hash 83702148)
 :void (symbol-id string))

(defgmethod
 (gdscript-text-document+didopen :class 'gdscript-text-document :bind "didOpen"
  :hash 1114965689)
 :void (params variant))

(defgmethod
 (gdscript-text-document+didclose :class 'gdscript-text-document :bind
  "didClose" :hash 1114965689)
 :void (params variant))

(defgmethod
 (gdscript-text-document+didchange :class 'gdscript-text-document :bind
  "didChange" :hash 1114965689)
 :void (params variant))

(defgmethod
 (gdscript-text-document+willsavewaituntil :class 'gdscript-text-document :bind
  "willSaveWaitUntil" :hash 1114965689)
 :void (params variant))

(defgmethod
 (gdscript-text-document+didsave :class 'gdscript-text-document :bind "didSave"
  :hash 1114965689)
 :void (params variant))

(defgmethod
 (gdscript-text-document+nativesymbol :class 'gdscript-text-document :bind
  "nativeSymbol" :hash 3762224011)
 variant (params dictionary))

(defgmethod
 (gdscript-text-document+documentsymbol :class 'gdscript-text-document :bind
  "documentSymbol" :hash 3877611628)
 array (params dictionary))

(defgmethod
 (gdscript-text-document+completion :class 'gdscript-text-document :bind
  "completion" :hash 3877611628)
 array (params dictionary))

(defgmethod
 (gdscript-text-document+resolve :class 'gdscript-text-document :bind "resolve"
  :hash 1333564645)
 dictionary (params dictionary))

(defgmethod
 (gdscript-text-document+rename :class 'gdscript-text-document :bind "rename"
  :hash 1333564645)
 dictionary (params dictionary))

(defgmethod
 (gdscript-text-document+preparerename :class 'gdscript-text-document :bind
  "prepareRename" :hash 3762224011)
 variant (params dictionary))

(defgmethod
 (gdscript-text-document+references :class 'gdscript-text-document :bind
  "references" :hash 3877611628)
 array (params dictionary))

(defgmethod
 (gdscript-text-document+foldingrange :class 'gdscript-text-document :bind
  "foldingRange" :hash 3877611628)
 array (params dictionary))

(defgmethod
 (gdscript-text-document+codelens :class 'gdscript-text-document :bind
  "codeLens" :hash 3877611628)
 array (params dictionary))

(defgmethod
 (gdscript-text-document+documentlink :class 'gdscript-text-document :bind
  "documentLink" :hash 3877611628)
 array (params dictionary))

(defgmethod
 (gdscript-text-document+colorpresentation :class 'gdscript-text-document :bind
  "colorPresentation" :hash 3877611628)
 array (params dictionary))

(defgmethod
 (gdscript-text-document+hover :class 'gdscript-text-document :bind "hover"
  :hash 3762224011)
 variant (params dictionary))

(defgmethod
 (gdscript-text-document+definition :class 'gdscript-text-document :bind
  "definition" :hash 3877611628)
 array (params dictionary))

(defgmethod
 (gdscript-text-document+declaration :class 'gdscript-text-document :bind
  "declaration" :hash 3762224011)
 variant (params dictionary))

(defgmethod
 (gdscript-text-document+signaturehelp :class 'gdscript-text-document :bind
  "signatureHelp" :hash 3762224011)
 variant (params dictionary))