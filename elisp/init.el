(setq debug-on-error 1)
(setq custom-file (make-temp-file ""))

(setq user-init-file load-file-name)

;; knowt user directory
(defun knowt-user-directory () (or (getenv "KNOWT_USER_DIRECTORY")
                                   (expand-file-name "~/.knowt/")))
(message (format "knowt-user-directory: %s" (knowt-user-directory)))

;; knowt emacs directory
(setq user-emacs-directory (concat (knowt-user-directory) ".emacs.d/"))
(message (format "user-emacs-directory set: %s" user-emacs-directory))

;; knowt source directory
(defun knowt-source-directory () (file-name-directory load-file-name))

(defun knowt-user-file (filename)
  (concat (knowt-user-directory) (file-name-nondirectory filename)))
(message (format "knowt-user-file: %s" (knowt-user-file "FOO")))

(defun knowt-user-org-file ()
  (concat (knowt-user-directory) "init.org"))
(message (format "knowt-user-org-file: %s" (knowt-user-org-file)))

(defun knowt-user-el-file ()
  (concat user-emacs-directory "init.el"))
(message (format "knowt-user-el-file: %s" (knowt-user-el-file)))

(defun knowt-org-file ()
  (concat (knowt-user-directory) "knowt.org"))
(message (format "knowt-org-file: %s" (knowt-org-file)))

(require 'org)

(defun knowt-build-core ()
  (let ((core-org (concat (knowt-source-directory) "core.org"))
        (core-el (concat user-emacs-directory "core.el")))
    (org-babel-tangle-file core-org core-el)
    (if (file-exists-p core-el)
        (load-file core-el))))

(defun knowt-build-custom ()
  (if (file-exists-p (knowt-user-org-file))
      (progn
        (org-babel-tangle-file (knowt-user-org-file) (knowt-user-el-file))
        (if (file-exists-p (knowt-user-el-file))
            (load-file (knowt-user-el-file))))))

(knowt-build-core)
(knowt-build-custom)

(find-file (knowt-org-file))
