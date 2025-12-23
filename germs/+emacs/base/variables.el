(defvar pathogen-emacs-base/cache-dir
  (cond ((getenv "PATHOGEN_CACHE_DIR") (getenv "PATHOGEN_CACHE_DIR"))
        (t (expand-file-name "cache/" pathogen--emacs-dir)))
  "Directory where Pathogen stores cache files.
This includes savehist, recentf, and other persistent data.
Users can customize this to store cache in a different location. Can be configured using the environment variable PATHOGEN_CACHE_DIR.")
