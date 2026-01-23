;;; "Compiled" snippets and support files for `powershell-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'powershell-mode
                     '(("par"
                        "[Parameter(Mandatory=$true, ValueFromPipelineByPropertyName=$true)][string]\\$${1:Foobar}"
                        "parameter" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/powershell-mode/parameter"
                        nil nil)
                       ("fun" "Function ${1:Verb-Noun} {\n$0\n}\n" "function"
                        nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/powershell-mode/function"
                        nil nil)
                       ("cbh"
                        "<#\n.SYNOPSIS\n${1:Brief description}\n\n.DESCRIPTION\n${2:Longer description}\n\n.PARAMETER Foobar\n${3:Descriptions of parameter Foobar}\n\n.EXAMPLE\n${4:Actual example}\n\n.NOTES\n${5:Additional notes}\n#>\n"
                        "comment-based-help" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/powershell-mode/comment-based-help"
                        nil nil)
                       ("cmd" "[cmdletbinding()]\nParam (\n  $0\n)"
                        "cmdletbinding" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/powershell-mode/cmdletbinding"
                        nil nil)))


;;; Do not edit! File generated at Wed Nov 26 12:33:38 2025
