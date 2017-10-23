#!/bin/bash
# Opens contents of clipboard
emacsclient -s ~/.emacs.d/server/server -e "(progn 
        (switch-to-buffer (generate-new-buffer \"clipboard\"))
        (yank)

       ;; Raise/focus our window; depends on the windowing system
       (if (string-equal system-type \"darwin\")
         (ns-do-applescript \"tell application \\\"Emacs\\\" to activate\")
         (raise-frame))

       )"
