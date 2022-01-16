(setq user-emacs-directory "/home/emiac/research/.emacs.d/")
(setq default-directory "/home/emiac/research")
(setenv "HOME" "/home/emiac")

;; Initialize package source
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; initialize use-package
(unless (package-installed-p 'use-package) (package-install 'use-package))

(use-package all-the-icons
  :commands all-the-icons-install-fonts
)