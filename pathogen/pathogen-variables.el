(defvar pathogen-config-file (substitute-in-file-name "$HOME/.pathogen.el")
  "User's personal configuration file.
This file is loaded after all Pathogen modules if it exists. Use this
for personal customizations without modifying Pathogen core files.")

(defvar pathogen-font-family "Inconsolata LGC Nerd Font"
  "The default font family.")

(defvar pathogen-font-size 150
  "The default font height.")

;;; Internal variables

(defvar pathogen--base-cache-dir
  (cond ((getenv "PATHOGEN_CACHE_DIR") (getenv "PATHOGEN_CACHE_DIR"))
        (t (expand-file-name "cache/" user-emacs-directory)))
  "Directory where Pathogen stores cache files.
This includes savehist, recentf, and other persistent data.
Users can customize this to store cache in a different location. Can be configured using the environment variable PATHOGEN_CACHE_DIR.")

(setq custom-file (expand-file-name
                   "custom.el"
                   pathogen--base-cache-dir))

;; Keep the .emacs.d clean from backups and auto-saves
(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" pathogen--base-cache-dir)))
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" pathogen--base-cache-dir) t)))

(provide 'pathogen-variables)
