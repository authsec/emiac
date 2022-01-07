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
(set-face-attribute 'default nil :font "Roboto Mono" :height 120)

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

(use-package svg-tag-mode
  :hook org-mode
  :config
  (require 'svg-tag-mode)

  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
		(svg-lib-progress-bar (/ (string-to-number value) 100.0)
				      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
		(svg-lib-tag (concat value "%")
			     nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
	   (count (float (car seq)))
	   (total (float (cadr seq))))
      (svg-image (svg-lib-concat
		  (svg-lib-progress-bar (/ count total) nil
					:margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
		  (svg-lib-tag value nil
			       :stroke 0 :margin 0)) :ascent 'center)))

  (setq svg-tag-tags
	`(
	  ;; Org tags
	  (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
	  (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

	  ;; Task priority
	  ("\\[#[A-Z]\\]" . ( (lambda (tag)
				(svg-tag-make tag :face 'org-priority 
					      :beg 2 :end -1 :margin 0))))

	  ;; Progress
	  ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
					      (svg-progress-percent (substring tag 1 -2)))))
	  ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
					    (svg-progress-count (substring tag 1 -1)))))

	  ;; TODO / DONE
	  ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0 :scale 1))))
	  ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


	  ;; Citation of the form [cite:@Knuth:1984] 
	  ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
					    (svg-tag-make tag
							  :inverse t
							  :beg 7 :end -1
							  :crop-right t))))
	  ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
						     (svg-tag-make tag
								   :end -1
								   :crop-left t))))


	  ;; Active date (without day name, with or without time)
	  (,(format "\\(<%s>\\)" date-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :end -1 :margin 0))))
	  (,(format "\\(<%s *\\)%s>" date-re time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
	  (,(format "<%s *\\(%s>\\)" date-re time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

	  ;; Inactive date  (without day name, with or without time)
	  (,(format "\\(\\[%s\\]\\)" date-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
	  (,(format "\\(\\[%s *\\)%s\\]" date-re time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
	  (,(format "\\[%s *\\(%s\\]\\)" date-re time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))))

  (svg-tag-mode t)

  ;; To do:         TODO DONE  
  ;; Tags:          :TAG1:TAG2:TAG3:
  ;; Priorities:    [#A] [#B] [#C]
  ;; Progress:      [1/3]
  ;;                [42%]
  ;; Active date:   <2021-12-24>
  ;;                <2021-12-24 14:00>
  ;; Inactive date: [2021-12-24]
  ;;                [2021-12-24 14:00]
  ;; Citation:      [cite:@Knuth:1984]
  )

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
  (shell-command (concat "ssh " (getenv "HOST_USER") "@" (getenv "HOST_IP") " -n \"open x-devonthink-item:" uid "\"")))

(defun browse-url-emiac-mac-host (url &optional _new-window)
  "Communicate with the EmIAC host and open the URL in the default 
   browser on the host.
   The host OS here is MacOS
  "
  (interactive (browse-url-interactive-arg "KDE URL: "))
  (message "Sending URL to Host OS...")
  (apply #'start-process "name" "foo" "ssh" (concat (getenv "HOST_USER") "@" (getenv "HOST_IP")) "-n" (list (concat "open " url))))
(setq browse-url-browser-function 'browse-url-emiac-mac-host)

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

(setq org-hugo-base-dir (concat emiac-home-dir "/research/export/hugo/dump"))

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
;; Set default zoom to fit the whole page
;; http://pragmaticemacs.com/emacs/more-pdf-tools-tweaks/
;; https://github.com/politza/pdf-tools/blob/master/lisp/pdf-view.el
(setq-default pdf-view-display-size 'fit-page)

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
