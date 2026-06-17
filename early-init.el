;;; early-init.el --- Early initialization file -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Victor Santos

;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; URL: https://codeberg.org/padawanphysicist/pathogen-emacs
;; Package-Requires: ((emacs "27.1"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs 27.1 introduced the early init file (called "early-init.el", see
;; `early-init-file'), which is loaded before the regular init file. This file
;; is loaded very early in the startup process, before the package manager and
;; graphical user interface (GUI) are initialized.

;; Core respnsibilities:
;;
;; 1. Control package initialization
;;    - Skipping default package loading: prevents the built-in `package.el' from
;;      scanning and activating extensions automatically. This is required if you
;;      use other package manager than the default.
;; 2. Override default variables:
;;    - Inhibit welcomes: silences splash screens, startup echo messages and
;;      default scratch text before they try to render.
;;    - Configure early paths: sets early environment variables or load paths
;;      before init file is loaded.
;; 3. Prevent unwanted flashing of visual artifacts
;;    - Early GUI stripping: turns off the menu bar, tool bar, and scroll bars
;;      before the initial frame is rendered.
;;    - Frame parameter pre-setting: defines the basic behaviour so the first
;;      frame loads in its correct desired state.
;; 4. Optimizing Startup Performance:
;;    - Garbage Collector tuning: temporarily defer garbage collector to the end
;;      of emacs startup, and tune it to not interferring during regular session.
;;    - File handler deferral: disable file name handlers (like magic file names)
;;      during startup to speed up the reading and loading of scripts.
;;
;; For most customizations, use the regular init.el file instead.

;; References:
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html
;; - https://lists.gnu.org/archive/html/emacs-devel/2020-08/msg00237.html
;; - https://www.gnu.org/savannah-checkouts/gnu/emacs/news/NEWS.27.1
;; - https://github.com/emacs-mirror/emacs/blob/master/etc/NEWS.27

;;; Code:

;;;; Defer package initialization

;; Prior to Emacs 27, the init file was responsible for initializing the package
;; manager by explicitly calling `package-initialize'. Starting with Emacs 27,
;; the default behavior changed: `package-initialize' is now automatically
;; called before loading the init file. This means package initialization occurs
;; after early init file is loaded but before init file is processed. To prevent
;; Emacs from initializing packages automatically, we set
;; `package-enable-at-startup' to nil:
(when (version<= "27" emacs-version)
  (setq package-enable-at-startup nil))

;;;; Warning level configuration

;; Set to :error instead of :emergency to avoid suppressing important warnings
;; about configuration issues, deprecated functions, or package loading
;; errors. This still prevents minor warnings from interrupting startup while
;; keeping you informed about actual problems.
;;
;; Warning levels (least to most severe):
;;   :debug < :info < :warning < :error < :emergency
;;
;; - :emergency suppresses almost everything (previous setting)
;; - :error shows errors but hides routine warnings (current setting)
;; - :warning shows all warnings (Emacs default, can be noisy)
(setq warning-minimum-level :error)

;;;; Add Pathogen directories to `load-path'

(dolist (pathogen-path
	 (list
	  (expand-file-name "pathogen/" user-emacs-directory)
	  (expand-file-name ".pathogen.d/" (getenv "HOME"))))
  (if (file-directory-p pathogen-path)
      (add-to-list 'load-path pathogen-path)
    (message (format "Directory %s does not exist. Skipping it." pathogen-path))))

;;;; Minimize interference of GC during Emacs session

;; The garbage collector (GC) of Emacs is very simple. When needed, Emacs
;; allocates some bytes during the sessio and once you pass a threshold, the
;; garbage collector is triggered.

;; It turns out that the default behaviour is to collect very often (you can
;; check whether or not this is the case foro you setting the variable
;; `garbage-collection-messages' to t). Usually there is so little garbage to
;; collect each time that you will not notive any lag. The problem is when you
;; use memory intensive features like `helm' on a large collection.

;; When GC is triggered it can eats up quite a bit of time, easily doubling
;; startup time. We reduce this initialization time by defering GC, turning up
;; the memory threshold as early as possible.

;; To control the trigger of the GC we can use the variables `gc-cons-threshold'
;; and `gc-cons-percentage'.

;; Therefore to improve GC we adopt the following strategy:
;;   a. Increase `gc-cons-threshold' to a large number, so GC is not triggered
;;      early during startup
;;   b. Restore it to a sane value after initializatio finishes.

;; Step 'a' is easy:
(defun pathogen--gc-defer ()
  "Defer garbage collection."
  (setf gc-cons-threshold most-positive-fixnum))
(pathogen--gc-defer)

;; For step 'b', we proceed as follows: since the default value of
;; `gc-cons-threshold' is 800Kb, we define new values for defaults:
(defcustom gc-cons-high-threshold 67108864 ; 64mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this. If you experience stuttering,
increase this.")

(defcustom gc-cons-high-percentage 0.5 ; 50%, default is 0.1
  "The default value to use for `gc-cons-percentage'.")

;; and add a function to restore GC as a hook:
(defun pathogen--gc-restore ()
  "Restore garbage collector."
  (setf gc-cons-threshold gc-cons-high-threshold)
  (setf gc-cons-percentage gc-cons-high-percentage))
(add-hook 'emacs-startup-hook #'pathogen--gc-restore)

;; we use the same strategy when using minibuffer:
(add-hook 'minibuffer-setup-hook #'pathogen--gc-defer)
(add-hook 'minibuffer-exit-hook #'pathogen--gc-restore)

;; we also add a hook to garbage collect only when emacs is out of focus:
(add-hook 'emacs-startup-hook (lambda ()
				(add-function :after after-focus-change-function
					      (lambda ()
						(unless (frame-focus-state)
						  (garbage-collect))))))

;;;; Early UI optimizations

;; To prevent the glimpse of unstyled Emacs we tweak some UI elements early:
(unless (eq system-type 'android)
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars . 0) default-frame-alist)
  (push '(horizontal-scroll-bars . 0) default-frame-alist)

  (setq menu-bar-mode nil
	tool-bar-mode nil
	scroll-bar-mode nil))

;; Android specific settings
(when (eq system-type 'android)
  ;; Ensure automatic appearance of virtual keyboard when editing buffers
  (setq touch-screen-display-keyboard t)
  (tool-bar-mode 1)
  (menu-bar-mode 1))

(provide 'early-init)

;;; early-init.el ends here

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; outline-regexp: ";;;+"
;; outline-minor-mode-use-buttons: t
;; outline-minor-mode-cycle: t
;; fill-column: 80
;; End:
