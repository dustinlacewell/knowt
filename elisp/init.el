;; source path - based on init file
;; user path - pased on environment variable
(setq user-init-file load-file-name)

(defun knowt-source-directory ()
  (file-name-directory user-init-file))
(message (format "knowt-source-directory: %s" (knowt-source-directory)))

(defun knowt-user-directory ()
  (file-name-as-directory (or (getenv "KNOWT_USER_DIRECTORY") (expand-file-name "~/.knowt/"))))
(message (format "knowt-user-directory: %s" (knowt-user-directory)))

(defun knowt-emacs-directory ()
  (file-name-as-directory (concat (knowt-user-directory) ".emacs.d/")))
(message (format "knowt-emacs-directory: %s" (knowt-emacs-directory)))

(defun knowt-emacs-file (filename)
  (concat (knowt-emacs-directory) (file-name-nondirectory filename)))
(message (format "knowt-emacs-file: %s" (knowt-emacs-file "FOO")))

(defun knowt-user-file (filename)
  (concat (knowt-user-directory) (file-name-nondirectory filename)))
(message (format "knowt-user-file: %s" (knowt-user-file "FOO")))

(defun knowt-mode-org-file ()
  (concat (knowt-source-directory) "knowt-mode.org"))
(message (format "knowt-mode-org-file: %s" (knowt-mode-org-file)))

(defun knowt-mode-el-file ()
  (concat (knowt-emacs-directory) "knowt-mode.el"))
(message (format "knowt-mode-el-file: %s" (knowt-mode-el-file)))

(defun knowt-user-org-file ()
  (concat (knowt-emacs-directory) "custom.org"))
(message (format "knowt-user-org-file: %s" (knowt-user-org-file)))

(defun knowt-user-el-file ()
  (concat (knowt-emacs-directory) "custom.el"))
(message (format "knowt-user-el-file: %s" (knowt-user-el-file)))

(defun knowt-build-el-file ()
  (concat (knowt-source-directory) "org-build.el"))
(message (format "knowt-build-el-file: %s" (knowt-build-el-file)))

(setq user-init-file load-file-name)
(setq user-emacs-directory (knowt-emacs-directory))

(load-file (knowt-build-el-file))

(require 'org-build)
(build-custom)
(build-mode)

(defun knowt-hook ()
  (when (string= (file-name-nondirectory (buffer-file-name)) "knowt.org")
    (knowt-mode 1)))

(add-hook 'find-file-hook 'knowt-hook)
(find-file (knowt-user-file "knowt.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (req-package use-package el-get))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
