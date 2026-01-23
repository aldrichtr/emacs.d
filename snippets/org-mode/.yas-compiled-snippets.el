;;; "Compiled" snippets and support files for `org-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("<vi"
                        "[[${1:link to the video}][file:${2:link of the image}]\n"
                        "video" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/video"
                        nil nil)
                       ("<v" "#+begin_verse\n$0\n#+end_verse" "verse" nil nil
                        nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/verse"
                        nil nil)
                       ("uml" "#+begin_uml\n$1\n#+end_uml" "uml" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/uml"
                        nil nil)
                       ("+TITLE" "#+TITLE: $0" "property - title" nil
                        ("File Properties") nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/title"
                        nil nil)
                       (":tangle"
                        ":tangle ${0:$$(yas-choose-value '(\"yes\" \"no\" \"\"))}"
                        "tangle" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/tangle"
                        nil nil)
                       ("tangld"
                        ":PROPERTIES:\n:ID: `(org-id-uuid)`\n:END:\n#+title: Tangld source for Emacs init.el\n#+subtitle: A literate configuration for Emacs\n#+PROPERTY: header-args  :session init\n#+PROPERTY: header-args+ :noweb yes\n#+PROPERTY: header-args+ :cache no\n#+PROPERTY: header-args+ :lexical yes\n#+PROPERTY: header-args+ :comments noweb\n#+PROPERTY: header-args+ :padline yes\n#+tangld_lib:\n\n* ${1:`(capitalize (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`}\n  :PROPERTIES:\n  :END:\n\n  #+NAME: ${1:$(downcase yas-text)}\n  #+BEGIN_SRC emacs-lisp\n  ${0:`(substring-no-properties (current-kill 0 t))`}\n  #+END_SRC\n\n"
                        "tangld-template" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/tangld-template"
                        nil nil)
                       ("<ta"
                        "#+caption: ${1: caption of the table}\n|${2:column 1} | ${3: column 2} |\n|--------------+----------------|\n"
                        "table" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/table"
                        nil nil)
                       ("+sub" "#+SUBTITLE: $0" "property - subtitle" nil nil
                        nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/subtitle"
                        nil nil)
                       ("<st"
                        "#+style: <link rel=\"stylesheet\" type=\"text/css\" href=\"$1\" />"
                        "style" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/style"
                        nil nil)
                       ("<src" "#+BEGIN_SRC $1\n  $0\n#+END_SRC\n" "src block"
                        nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/src"
                        nil nil)
                       ("set" "#+setupfile: $0" "setup" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/setup"
                        nil nil)
                       ("<rib"
                        "    :properties:\n    :reveal_background: ${1: path of the image}\n    :reveal_background_trans: ${2: default||cube||page||concave||zoom||linear||fade||none||slide}\n    :end:"
                        "reveal_image_background" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/rv_image_background"
                        nil nil)
                       ("<rsb"
                        ":properties:\n:reveal_background: ${1: #123456}\n:end:"
                        "reveal_single_colored_background" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/rv_background"
                        nil nil)
                       ("<q" "#+begin_quote\n$0\n#+end_quote" "quote" nil nil
                        nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/quote"
                        nil nil)
                       ("py_" "#+begin_src python\n$0\n#+end_src" "python" nil
                        nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/python"
                        nil nil)
                       (":prop-drawer" ":PROPERTIES:\n$0\n:END:\n"
                        "Property-Drawer" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/property-drawer"
                        nil nil)
                       ("<op"
                        "#+options: h:${1:1} num:${2:t||nil} toc:${3:t||nil}$0"
                        "options" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/options"
                        nil nil)
                       (":noweb"
                        ":noweb ${0:$$(yas-choose-value '(\"yes\" \"no\" \"\"))}"
                        "noweb" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/noweb"
                        nil nil)
                       ("name" "#+NAME: `yas-selected-text`$0\n" "name property"
                        nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/name"
                        nil nil)
                       ("matrix_"
                        "\\left \\(\n\\begin{array}{${1:ccc}}\n${2:v1 & v2} \\\\\n$0\n\\end{array}\n\\right \\)"
                        "matrix" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/matrix"
                        nil nil)
                       ("<li" "[[${1:link}][${2:description}]]\n" "link" nil nil
                        nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/link"
                        nil nil)
                       ("<lan" "#+language: ${1:en}" "language" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/language"
                        nil nil)
                       ("<ke" "#+keywords: $0" "keywords" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/keywords"
                        nil nil)
                       ("ipy_"
                        "#+begin_src ipython :session ${1:session01} :file ${2:$$(concat (make-temp-name \"./ipython-\") \".png\")} :exports ${3:both}\n$0\n#+end_src"
                        "ipython" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/ipython"
                        nil nil)
                       ("<i" "#+include: $0" "include" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/include"
                        nil nil)
                       ("img_"
                        "<img src=\"$1\" alt=\"$2\" align=\"${3:left}\" title=\"${4:image title}\" class=\"img\" $5/>$0"
                        "img" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/img"
                        nil nil)
                       ("<im"
                        "#+caption: ${1:caption of the image}\n[[file:${2:image_path}]]$0"
                        "image" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/image"
                        nil nil)
                       ("<ht" "#+html:$1" "html" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/html"
                        nil nil)
                       ("header-args" "#+PROPERTY: header-args $0\n"
                        "header-args" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/header-args"
                        nil nil)
                       ("filetags" "#+filetags: $0\n" "filetags property" nil
                        nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/filetags"
                        nil nil)
                       ("fig_"
                        "#+caption: ${1:caption}\n#+attr_latex: ${2:scale=0.75}\n#+label: fig:${3:label}$0"
                        "figure" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/figure"
                        nil nil)
                       ("<ex" "#+begin_export ${1:type}\n$0\n#+end_export"
                        "export" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/export"
                        nil nil)
                       ("<e" "#+begin_example\n$0\n#+end_example" "example" nil
                        nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/exampleblock"
                        nil nil)
                       ("entry_"
                        "#+begin_html\n---\nlayout: ${1:default}\ntitle: ${2:title}\n---\n#+end_html\n"
                        "entry" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/entry"
                        nil nil)
                       ("emb_" "src_${1:lang}${2:[${3:where}]}{${4:code}}"
                        "embedded" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/embedded"
                        nil nil)
                       ("<em" "#+email: $0" "email" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/email"
                        nil nil)
                       ("emacs-lisp_"
                        "#+BEGIN_SRC emacs-lisp :tangle yes\n$0\n#+END_SRC"
                        "emacs-lisp" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/emacs-lisp"
                        nil nil)
                       ("elisp" "#+BEGIN_SRC emacs-lisp\n$0\n#+END_SRC" "elisp"
                        nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/elisp"
                        nil nil)
                       ("dot_"
                        "#+begin_src dot :file ${1:file} :cmdline -t${2:pdf} :exports none :results silent\n$0\n#+end_src\n[[file:${3:path}]]"
                        "dot" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/dot"
                        nil nil)
                       ("desc" "#+DESCRIPTION: $0" "description" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/description"
                        nil nil)
                       ("<da" "#+date: ${1:year}:${2:month}:${3:day}" "date" nil
                        nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/date"
                        nil nil)
                       ("<c" "#+begin_center\n$0\n#+end_center" "center" nil nil
                        nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/center"
                        nil nil)
                       ("<au" "#+AUTHOR: $0" "author" nil nil nil
                        "c:/Users/aldrichtr/AppData/Roaming/emacs/snippets/org-mode/author"
                        nil nil)))


;;; Do not edit! File generated at Wed Nov 26 12:33:38 2025
