#!/bin/bash
# From https://developer.atlassian.com/blog/2015/03/emacs-intellij/
# Opens file in Emacs in given position.
#
file=$1
line=$2
col=$3
emacsclient -s ~/.emacs.d/server/server -q -e "(progn
       (find-file \"$file\")

       ;; Raise/focus our window; depends on the windowing system
       (if (string-equal system-type \"darwin\")
        (ns-do-applescript \"tell application \\\"Emacs\\\" to activate\")
        (raise-frame))

       (when (not (string= \"\" \"$line\"))
        (goto-char (point-min))
        (forward-line (1- $line))
        (forward-char (1- $col)))

        (auto-revert-mode t)
)"
