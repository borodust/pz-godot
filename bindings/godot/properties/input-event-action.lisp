(common-lisp:in-package :%godot)


(defgproperty input-event-action+action 'input-event-action :get
 'input-event-action+get-action :set 'input-event-action+set-action)

(defgproperty input-event-action+pressed 'input-event-action :set
 'input-event-action+set-pressed)

(defgproperty input-event-action+strength 'input-event-action :get
 'input-event-action+get-strength :set 'input-event-action+set-strength)

(defgproperty input-event-action+event-index 'input-event-action :get
 'input-event-action+get-event-index :set 'input-event-action+set-event-index)