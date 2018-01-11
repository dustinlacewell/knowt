;;; Code:

(require 'org)
(require 'subr-x)
(require 'find-lisp)

;;;; Basic Functions

(defun find-tmpfile-path ()
  "Find suitable tmpfile path."
  (let ((my-ramdisk "/Volumes/ramdisk"))
    ;; I always create `/Volumes/ramdisk' for ramdisk usage on OSX
    (if (and (eq system-type 'darwin) (file-exists-p my-ramdisk))
        my-ramdisk
        temporary-file-directory)))

(defun dirjoin (root &rest dirs)
  "Joins ROOT and DIRS together."
  (if (not dirs)
      root
      (apply 'dirjoin
             (expand-file-name (car dirs) root)
             (cdr dirs))))

;;;; Variables

(defconst *workdir* (dirjoin (find-tmpfile-path) ".emacs-init-build")
  "Working directory for building emacs-lisp configs.")

(defconst *pid* (number-to-string (emacs-pid))
  "Current process pid in string.")

(defconst *lockfile* (dirjoin *workdir* "lock")
  "A file store pid info.")

;;;; Functions

(defun mkdir-1 (dirname)
  "Create directory DIRNAME and it's parents."
  (when (not (file-exists-p dirname))
    (make-directory dirname :parents)))

(defun find-org-files (dirname)
  "Return a list store all .org file in DIRNAME."
  (thread-last (find-lisp-find-files dirname "^[^\\.#].*\\.org$")
    (mapcar (lambda (f) (replace-regexp-in-string (expand-file-name user-emacs-directory) "" f)))))

(defun find-tmp-el-files ()
  "Return a list store all .pid files."
  (find-lisp-find-files *workdir* (concat "\\." *pid* "$")))

(defun delete-tmp-files ()
  "Delete all temp file generate by this script."
  (dolist (f (find-tmp-el-files))
    (delete-file f nil)))

(defun org->el (forg)
  "Convert `org-mode' file FORG to emacs-lisp file in *workdir*."
  (let* ((dir (dirjoin *workdir* (or (file-name-directory forg) "")))
         (base (file-name-base forg))
         (fel (dirjoin dir (concat base ".el" "." *pid*))))
    (mkdir-1 dir)
    (message (format "Building %s to %s ..." forg fel))
    (org-babel-tangle-file forg fel)))

(defun write-lockfile (pid)
  "Create lockfile with PID as it's contents."
  (mkdir-1 (file-name-directory *lockfile*))
  (with-temp-buffer
    (insert pid)
    (write-file *lockfile*)))

(defun read-lockfile ()
  "Return lockfile contents."
  (with-temp-buffer
    (insert-file-contents *lockfile*)
    (buffer-string)))

(defun update-config (forg)
  "Update FORG's relative .el file from *workdir*."
  (let* ((bdir (or (knowt-emacs-directory) ""))
         (base (file-name-base forg))
         (dir  (file-name-directory forg))
         (fel  (dirjoin dir (format "%s.el.%s" base *pid*)))
         (fel2 (dirjoin bdir (concat base ".el"))))
    (message (format "Create %s from %s..." fel2 fel))
    (rename-file fel fel2 t)))

(defun check-if-process-end ()
  "Check if current process's pid match to `*lockfile*', remove all tmp files and exit when failed."
  (unless (string= *pid* (read-lockfile))
    (message "Current pid not match to *lockfile*, exit.")
    (delete-tmp-files)
    (kill-emacs)))

(defun org-build (filename)
  "Build all .org configs to emacs-lisp file."
  (setq org-confirm-babel-evaluate nil)
  ;; Create lockfile with pid
  (write-lockfile *pid*)
  ;; if current process pid not match to lockfile, remove tmpfiles and exist.
  (check-if-process-end)
  ;; tangle org-mode files.
  (org->el filename)
  ;; check if current process pid match to lock file again
  (check-if-process-end)
  (update-config filename))

(defun build-mode ()
  (org-build (knowt-mode-org-file))
  (load-file (knowt-mode-el-file)))

(defun build-custom-org ()
  (org-build (knowt-user-org-file)))

(defun build-custom ()
  (if (file-exists-p (knowt-user-org-file))
      (progn
        (build-custom-org)
        (if (file-exists-p (knowt-user-el-file))
            (load-file (knowt-user-el-file))))))

(provide 'org-build)
