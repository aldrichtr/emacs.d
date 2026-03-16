;;; custom.el --- Customization file -*- lexical-binding: t; -*-

;;; Commentary:
;; I have Emacs update customizations here vice in my `init.el'

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("972f792651d32b0506481b9e87b2fbc9b732ae9da2527562668c6e7d149fefda"
     "d0a8151d15ae39633c82b48fa2381845f89b8d7607467580a6e827984ecdc2af" default))
 '(package-selected-packages
   '(ample-zen-theme backline badger-theme beacon breadcrumb buffer-move buttercup
                     cape casual centaur-tabs citre cl-generic clj-refactor
                     clojure-ts-mode colonoscopy-theme colorful-mode cond-star
                     consult-gh-embark consult-gh-forge consult-lsp
                     consult-org-roam consult-projectile consult-todo
                     consult-yasnippet corfu csproj-mode darktooth-theme
                     dashboard deft dired-auto-readme dired-gitignore dirvish
                     doom-modeline doom-themes doric-themes dotnet drag-stuff
                     editorconfig ef-themes eglot eldoc-overlay elnode erc
                     evil-avy evil-collection evil-commentary evil-iedit-state
                     evil-lion evil-multiedit evil-numbers faceup
                     fancy-compilation flycheck-clj-kondo flycheck-hl-todo
                     flycheck-posframe general git-modes harpoon helpful
                     highlight-indent-guides info-colors inspector
                     jetbrains-darcula-theme json-mode komorebi let-alist
                     lsp-treemacs lsp-ui magit-org-todos magit-todos
                     major-mode-hydra marginalia mermaid-ts-mode multi-term
                     navi-mode nerd-icons-completion nerd-icons-corfu
                     nerd-icons-dired nnhackernews ntlm nushell-ts-mode
                     ob-powershell ob-rust orderless org-cliplink
                     org-clock-today org-context org-download org-gcal
                     org-kanban org-mem org-modern org-modern-indent org-ql
                     org-recur org-roam-ui org-transclusion outline-yaml
                     outshine parinfer-rust-mode powershell-ts-mode
                     projectile-ripgrep python rainbow-delimiters restart-emacs
                     sideline-flycheck so-long soap-client sublime-themes
                     surround svg system-packages term-projectile timeout
                     toggle-term tramp treemacs-evil treemacs-nerd-icons
                     treemacs-projectile treesit-auto undo-fu use-package
                     verilog-mode vertico-posframe which-key-posframe
                     window-tool-bar winum writeroom-mode ws-butler
                     yasnippet-capf yequake zenburn-theme zprint-mode))
 '(powershell-indent-level 2 nil nil "Customized with use-package powershell")
 '(safe-local-variable-values
   '((elisp-lint-indent-specs (describe . 2) (it . 2) (expect . 2))
     (elisp-lint-ignored-validators "byte-compile")
     (eval add-hook 'compilation-filter-hook
           (lambda nil
             (ignore-errors
               (require 'ansi-color)
               (when (eq major-mode 'compilation-mode)
                 (ansi-color-apply-on-region compilation-filter-start
                                             (point-max))))))
     (projectile-project-test-suffix "_test"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

;;; custom.el ends here
