;;; config-eglot-powershell.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:

(require 'f)
(require 'eglot)

(defun config/eglot-build-pses-command ()
  "Combine flags and paths into the PSES start command."
  (let* ((pwsh-exe (or (executable-find "pwrh.exe") (executable-find
                                                     "powershell.exe")))
         (pses-root (f-join (getenv "XDG_DATA_HOME") "lsp" "pses"))
         (pses-mod-root (f-join pses-root "PowerShellEditorServices"))
         (pses-module (f-join pses-mod-root "PowerShellEditorServices.psd1"))
         (log-root  (f-join pses-root "logs"))
         (log-level "Trace")
         (session-log (f-join log-root (format "eglot-pses-session-%s.json"
                                               (emacs-pid)))))
    (list pwsh-exe
          "-NoProfile" "-ExecutionPolicy" "Bypass"
          "-Command"
          "Import-Module" pses-module ";"
          "Start-EditorServices"
          "-HostName" "'Eglot'"
          "-HostVersion" "1.21"
          "-HostProfileId" "'GNU.Emacs'"
          "-BundledModulesPath" (format "'%s'" pses-root)
          ;; "-EnableConsoleRepl"
          "-StartupBanner" "'Eglot PowerShell LSP'"
          "-LogLevel" log-level
          "-LogPath" (format "'%s'" log-root)
          "-SessionDetailsPath" (format "'%s'" session-log)
          "-Stdio"
          "-FeatureFlags" "@()")))

(defun config/eglot-register-powershell ()
  "Register the PowerShell Editor Services LSP."
  (interactive)
  ;; Testing
  (setopt eglot-sync-connect t)
  (let ((start-script (config/eglot-build-pses-command)))
    (message "Eglot: registering PSES.  Server start script:\n%s\n" start-script)
    (add-to-list 'eglot-server-programs `((powershell-ts-mode powershell-mode)
                                          . ,start-script))))

(provide 'config-eglot-powershell)


;;; config-eglot-powershell.el ends here
