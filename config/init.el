(setq emiac-home-dir "/home/emiac")

(setq inhibit-startup-message t) ;Don't show the start screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(scroll-bar-mode -1) ; Disable the visible scrollbar
(tool-bar-mode -1)    ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room

(menu-bar-mode -1)   ; Disable the menubar

(setq visible-bell t); Setup visible bell
(show-paren-mode 1)  ; Highlight matching brackets (or braces/parenthesis)

;; Set up the key to the left of "end" to delete forward (might not be necessary if not running in a docker container on a Mac accessed through XQuartz)
(normal-erase-is-backspace-mode 1)

;; Setup a font
(set-face-attribute 'default nil :font "Roboto Mono" :height 140)

;; make backup to a designated dir, mirroring the full path

(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* (
	 (backupRootDir (concat emiac-home-dir "/.emacs.d/backup/"))
	 (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, for example, “C:”
	 (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") ))
	 )
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath
    )
  )

(setq make-backup-file-name-function 'my-backup-file-name)

(global-visual-line-mode t)

(global-hl-line-mode t)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Initialize package source
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; initialize use-package
(unless (package-installed-p 'use-package) (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package nano-theme)
(use-package nano-modeline)
(use-package nano-agenda)

;; Load modeline after init, as this was causing problems if immediately loaded
(add-hook 'after-init-hook #'nano-modeline-mode)
(load-theme 'nano-light t)

(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init (unless (find-font (font-spec :name "all-the-icons"))
	  (all-the-icons-install-fonts t)))
(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package command-log-mode)

;; Install sensible dependencies
(use-package swiper
  :ensure t
  )
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)
	 )
  )
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 )
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init (ivy-rich-mode 1)
  )

;; enable line numbering
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for selected modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0)))
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  )

(use-package general
  :config
  (general-create-definer authsec/leader-key
    :prefix "A-C-M-SPC"
    )
  )

(authsec/leader-key
  "b" 'counsel-bookmark
  "s" 'org-attach-screenshot
  )

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(setq org-display-inline-images t)
(setq org-redisplay-inline-images t)
(setq org-startup-with-inline-images "inlineimages")
(use-package org
  :custom
  (org-ellipsis " ⮷")
  :bind(
	("C-c a" . org-agenda)
	("C-c c" . org-capture)
	("C-c l" . org-store-link)
	)
  )
;; Store new notes at the beginning of the file
(setq org-reverse-note-order t)

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (dot . t)
     (emacs-lisp . t)
     (plantuml . t)
     (python . t)
     (shell . t)
     (sql . t)
     (latex . t)
     )
   )

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(setq org-confirm-babel-evaluate nil)

(setq org-babel-python-command "/usr/bin/python3")

(use-package org-special-block-extras
  :ensure t
  :after org
  :hook (org-mode . org-special-block-extras-mode)
  ;; All relevant Lisp functions are prefixed ‘o-’; e.g., `o-docs-insert'.

  :config
  (o-defblock noteblock (title "Note") (title-color "primary")
	      "Define noteblock export for docsy ox hugo"
	      (apply #'concat
		     (pcase backend
		       (`latex `("\\begin{noteblock}", contents, "\\end{noteblock}"))
		       (`hugo `("{{% alert title=\"", title, "\" color=\"", title-color, "\" %}}\n", contents, "\n{{% /alert %}}"))
		       )
		     )
	      )
  (o-defblock cautionblock (title "Caution") (title-color "warning")
	      "Awesomebox caution"
	      (apply #'concat
		     (pcase backend
		       (`latex `("\\begin{cautionblock}", contents, "\\end{cautionblock}"))
		       (`hugo `("{{% alert title=\"", title, "\" color=\"", title-color, "\" %}}\n", contents, "\n{{% /alert %}}"))
		       )
		     )
	      )
  )

;; (defun ox-mybackend-special-block ( special-block contents info )
;;   (let ((org-export-current-backend 'md))
;;          (org-hugo-special-block special-block contents info)))

;;      (advice-add 'org-hugo-special-block :around
;;       (lambda (f &rest r)
;; 	(let ((org-export-current-backend 'hugo))
;; 	  (apply 'f r))))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("java" . "src java"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(setq org-agenda-files
      '("~/research/org/tasks.org"))
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-capture-templates
      '(("t" "Todo" entry (file+olp+datetree "~/research/org/tasks.org")
	 "* TODO %?\n  %i\n  %a")))

(setq org-todo-keywords
      '(
	(sequence "TODO(t)" "NEXT(n)" "DAILY(a)" "|" "DONE(d)")
	(sequence "CONTACT(c)" "WAITING_FOR_RESPONSE(w)" "|" "DONE(d)")
	)

      )

(setq org-tag-alist
      '((:startgroup)
	;; Put mutually exclusive tags here
	(:endgroup)
	("email" . ?e)
	("phone" . ?p)
	("message" . ?m)
	)
      )

(org-add-link-type "x-devonthink-item" 'org-devonthink-item-open)
(defun org-devonthink-item-open (uid)
  "Open the given uid, which is a reference to an item in Devonthink"
  (shell-command (concat "open \"x-devonthink-item:" uid "\"")))

(setq my-roam-directory (concat (getenv "HOME") "/research/roam-notes"))
(setq org-roam-v2-ack t)
(use-package org-roam
  :ensure t
  :custom
  ;; make sure this directory exists
  (org-roam-directory (file-truename my-roam-directory))
  ;; configure the folder where dailies are stored, make sure this exists as well
  (org-roam-dailies-directory "dailies")
  ;; Lets you use completion-at-point
  (org-roam-completion-everywhere t)
  ;; (org-roam-graph-executable "~/bin/dot")
  :bind(
	("C-c n l" . org-roam-buffer-toggle)
	("C-c n f" . org-roam-node-find)
	("C-c n i" . org-roam-node-insert)
	:map org-mode-map
	("C-M-i" . completion-at-point)
	:map org-roam-dailies-map
	("Y" . org-roam-dailies-capture-yesterday)
	("T" . org-roam-dailies-capture-tomorrow)
	)
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure keymap is available
  (org-roam-setup)
  (org-roam-db-autosync-mode)
  )
;; Mapping mouse click to preview does not seem to work
;;(define-key org-roam-mode-map [mouse-1] #'org-roam-preview-visit)

(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)) ; optional: if Org Ref is not loaded anywhere else, load it here

(setq org-roam-capture-templates
      '(
	("d" "default" plain "%?"
	 :target (file+head "%<%Y%m%d%H%M%S>-${slug}/%<%Y%m%d%H%M%S>-${slug}.org"
			    "#+title: ${title}\n") :unnarrowed t)
	("w" "work" plain "%?"
	 :target (file+head "work/%<%Y%m%d%H%M%S>-${slug}/%<%Y%m%d%H%M%S>-${slug}.org"
			    "#+title: ${title}\n") :unnarrowed t)
	)
      )

(defun authsec-create-missing-directories-h ()
  "Automatically create missing directories when creating new files."
  (unless (file-remote-p buffer-file-name)
    (let ((parent-directory (file-name-directory buffer-file-name)))
      (and (not (file-directory-p parent-directory))
	   (y-or-n-p (format "Directory `%s' does not exist! Create it?"
			     parent-directory))
	   (progn (make-directory parent-directory 'parents)
		  t)))))
(add-hook 'find-file-not-found-functions #'authsec-create-missing-directories-h)

;; This advice automatically answers 'yes' or rather 'y' for the above function and therefore always creates the directory and places the .org file created by org-roam inside that directory.
;; The problem with the above approach however is that the directory gets created even if you later decide to abort your capture.
(defadvice authsec-create-missing-directories-h (around auto-confirm compile activate)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t))
	    ((symbol-function 'y-or-n-p) (lambda (&rest args) t)))
    ad-do-it))

(use-package ox-hugo
  :ensure t            ;Auto-install the package from Melpa (optional)
  :after ox)

(use-package toml-mode
  :ensure t)

(setq hugo-base-dir "~/research/export/hugo/dump")

(use-package deft
  :config
  (setq deft-directory my-roam-directory
	deft-recursive t
	deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
	deft-use-filename-as-title t)
  :bind
  ("C-c n s" . deft))

(setq org-latex-pdf-process
      (list
       "latexmk -interaction=nonstopmode -shell-escape -pdf -f %b.tex && latexmk -c -bibtex && rm -rf %b.run.xml %b.tex %b.bbl _minted-*"
       ))

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-minted-options '(("breaklines" "true")
				 ("breakanywhere" "true"))
      )

(use-package ivy-bibtex)

;; use the newer biblatex
(add-to-list 'org-latex-packages-alist '("backend=biber,sortlocale=de" "biblatex"))

;;setup dialect to be biblatex as bibtex is quite a bit old
(setq bibtex-dialect 'biblatex)
;; variables that control bibtex key format for auto-generation
;; I want firstauthor-year-title-words
;; this usually makes a legitimate filename to store pdfs under.
(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(eval-after-load 'ox-latex
  '(add-to-list 'org-latex-classes
		'("koma-article"
		  "\\documentclass{scrartcl}"
		  ("\\section{%s}" . "\\section*{%s}")
		  ("\\subsection{%s}" . "\\subsection*{%s}")
		  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		  ("\\paragraph{%s}" . "\\paragraph*{%s}")
		  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(eval-after-load 'ox-latex
  '(add-to-list 'org-latex-classes
		'("koma-report"
		  "\\documentclass{scrreprt}"
		  ("\\chapter{%s}" . "\\chapter*{%s}")
		  ("\\section{%s}" . "\\section*{%s}")
		  ("\\subsection{%s}" . "\\subsection*{%s}")
		  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		  ("\\paragraph{%s}" . "\\paragraph*{%s}")
		  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(eval-after-load 'ox-latex
  '(add-to-list 'org-latex-classes
		'("koma-book"
		  "\\documentclass{scrbook}"
		  ("\\chapter{%s}" . "\\chapter*{%s}")
		  ("\\section{%s}" . "\\section*{%s}")
		  ("\\subsection{%s}" . "\\subsection*{%s}")
		  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		  ("\\paragraph{%s}" . "\\paragraph*{%s}")
		  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package pdf-tools
  :config
  (pdf-tools-install t)
  )

;; Don't open .pdf files with an external viewer
(push '("\\.pdf\\'" . emacs) org-file-apps)
;; Don't ask if the PDF buffer should be replace with the newly created PDF
(setq revert-without-query '(".pdf"))

(use-package git-auto-commit-mode)
;;(setq gac-automatically-push-p t)
;;(setq gac-automatically-add-new-files-p t)
;; Commit/Push every 5 minutes
;;(setq gac-debounce-interval 300)
(custom-set-variables
 '(safe-local-variable-values '((setq gac-debounce-interval 300)))
 )

(require 'ob-plantuml)
(setq org-plantuml-jar-path "/usr/local/plantuml/plantuml.jar")
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
