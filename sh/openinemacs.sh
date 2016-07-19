#!/bin/bash
# From https://developer.atlassian.com/blog/2015/03/emacs-intellij/
# Opens file in Emacs in given position.
#
file=$1
line=$2
col=$3
/usr/bin/emacsclient -e "(progn 
       (find-file \"$file\")

       ;; Jump to the same point as in IntelliJ
       ;; Unfortunately, IntelliJ doesn't always supply the values
       ;; depending on where the open is invoked from; e.g. keyboard 
       ;; works, tab context doesn't
       (when (not (string= \"\" \"$line\"))
         (goto-char (point-min))
         (forward-line (1- $line))
         (forward-char (1- $col)))

       ;; Raise/focus our window; depends on the windowing system
       (if (string-equal system-type \"darwin\")
         (ns-do-applescript \"tell application \\\"Emacs\\\" to activate\")
         (raise-frame))
​
       ;; Automatically pick up changes made in IntelliJ
       (auto-revert-mode t))"
