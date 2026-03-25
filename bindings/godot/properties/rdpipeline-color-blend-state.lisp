(common-lisp:in-package :%godot)


(defgproperty rdpipeline-color-blend-state+enable-logic-op
 'rdpipeline-color-blend-state :get
 'rdpipeline-color-blend-state+get-enable-logic-op :set
 'rdpipeline-color-blend-state+set-enable-logic-op)

(defgproperty rdpipeline-color-blend-state+logic-op
 'rdpipeline-color-blend-state :get 'rdpipeline-color-blend-state+get-logic-op
 :set 'rdpipeline-color-blend-state+set-logic-op)

(defgproperty rdpipeline-color-blend-state+blend-constant
 'rdpipeline-color-blend-state :get
 'rdpipeline-color-blend-state+get-blend-constant :set
 'rdpipeline-color-blend-state+set-blend-constant)

(defgproperty rdpipeline-color-blend-state+attachments
 'rdpipeline-color-blend-state :get
 'rdpipeline-color-blend-state+get-attachments :set
 'rdpipeline-color-blend-state+set-attachments)