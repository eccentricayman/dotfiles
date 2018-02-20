;;; init.el --- eccentricayman's Emacs configuration

;;; Commentary:
;;; Move between config sections via C-x [ and C-x ]
;;; All backups in ~/.emacs.d/backups
;;; C-c w resizes window
;;; C-c s opens speedbar
;;; C-c-f Flycheck current buffer
;;; Run: C-c-j/h/p/c (java/browser/python/gcc)
;;; M-RET fullscreen

;;; Code:

;;more memory for garbage collection for init
(setq gc-cons-threshold 1000000000)

(require 'package)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (require 'cl)
  (require 'cl-lib))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-safe-themes
   (quote
	("503385a618581dacd495907738719565243ab3e6f62fec8814bade68ef66e996")))
 '(fci-rule-color "#3E4451")
 '(org-startup-truncated t)
 '(package-archives
   (quote
	(("gnu" . "http://elpa.gnu.org/packages/")
	 ("melpa" . "http://melpa.org/packages/")
	 ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(package-selected-packages
   (quote
	(all-the-icons-dired s latex-pretty-symbols auctex try powerline all-the-icons window-numbering page-break-lines smex ido-vertical-mode ido-better-flex windresize markdown-mode sr-speedbar flycheck multiple-cursors rainbow-delimiters swiper nlinum smartparens rjsx-mode web-mode jedi ac-c-headers fuzzy auto-complete atom-one-dark-theme diminish use-package)))
 '(vc-annotate-background "#3b3b3b")
 '(vc-annotate-color-map
   (quote
	((20 . "#dd5542")
	 (40 . "#CC5542")
	 (60 . "#fb8512")
	 (80 . "#baba36")
	 (100 . "#bdbc61")
	 (120 . "#7d7c61")
	 (140 . "#6abd50")
	 (160 . "#6aaf50")
	 (180 . "#6aa350")
	 (200 . "#6a9550")
	 (220 . "#6a8550")
	 (240 . "#6a7550")
	 (260 . "#9b55c3")
	 (280 . "#6CA0A3")
	 (300 . "#528fd1")
	 (320 . "#5180b3")
	 (340 . "#6380b3")
	 (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-doctype-face ((t (:foreground "#C678DD"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#E06C75"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "Grey"))))
 '(web-mode-html-tag-face ((t (:foreground "#61AFEF")))))

;;use-package install and diminsher setpu
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)

(use-package diminish
  :ensure t)


;;; Theme
(use-package atom-one-dark-theme
  :ensure t)
(load-theme 'atom-one-dark)

;;this is for regular emacs background
(defun emacs-terminal-background ()
  "Set background for terminal Emacs."
  (unless (display-graphic-p)
	(set-face-background 'default "222" (selected-frame))))
(add-hook 'window-setup-hook 'emacs-terminal-background)

;;this is for emacsclient background
(defun emacs-background-frame-config (frame)
  "Hook Emacs terminal background settings into current FRAME."
  (interactive)
  (with-selected-frame frame
	(unless (display-graphic-p)
	  (emacs-terminal-background))))
;; run now
(emacs-background-frame-config (selected-frame))
;; and later
(add-hook 'after-make-frame-functions 'emacs-background-frame-config)

;;hide the default gnu startup help message
(defun display-startup-echo-area-message ()
  "Hide default GNU startup help message."
  (message ""))


;;remove border
(set-face-attribute 'mode-line nil
                    :background "#21242b"
                    :foreground "#a0a5b4"
                    :box '(:line-width 2 :color "#282c34")
                    :overline nil
                    :underline nil)

;;change inactive colors for terminal
(if (display-graphic-p)
	(set-face-attribute 'mode-line-inactive nil
						:background "#23282d"
						:foreground "#5a646e"
						:box '(:line-width 2 :color "#282c34")
						:overline nil
						:underline nil)
  (set-face-attribute 'mode-line-inactive nil
					  :background "#555"
					  :foreground "#777"
					  :box '(:line-width 2 :color "#555")
					  :overline nil
					  :underline nil)
  )

;;powerline colors
(if (display-graphic-p)
	(progn
	  ;;custom powerline colors for gui
	  (defface ayman-powerline-active2 '((t (:background "#282C34" :foreground "#ABB2BF" :inherit mode-line)))
		"Powerline face 2."
		:group 'ayman-powerline)
	  (defface ayman-powerline-active1 '((t (:background "#3b4553" :foreground "#ABB2BF" :inherit mode-line)))
		"Powerline face 1."
		:group 'ayman-powerline)
	  (defface ayman-powerline-active0 '((t (:background "#21242b" :foreground "#ABB2BF" :inherit mode-line)))
		"Powerline face 0."
		:group 'ayman-powerline)
	  )
  (progn
	;;custom powerline colors for terminal
	(defface ayman-powerline-active2 '((t (:background "222" :foreground "white" :inherit mode-line)))
	  "Powerline face 2."
	  :group 'ayman-powerline)
	(defface ayman-powerline-active1 '((t (:background "#444" :foreground "white" :inherit mode-line)))
	  "Powerline face 1."
	  :group 'ayman-powerline)
	(defface ayman-powerline-active0 '((t (:background "#333" :foreground "white" :inherit mode-line)))
	  "Powerline face 0."
	  :group 'ayman-powerline)
	)
  )

(defun powerline-ayman-modified ()
  "Show padlock if read-only, chain for saved/unsaved."
  (let* ((config-alist
		  '(("*" all-the-icons-faicon-family all-the-icons-faicon "times" :height 1.0 :v-adjust 0.0)
			("-" all-the-icons-faicon-family all-the-icons-faicon "check" :height 1.0 :v-adjust 0.0)
			("%" all-the-icons-faicon-family all-the-icons-faicon "lock" :height 1.0 :v-adjust 0.0)))
		 (result (cdr (assoc (format-mode-line "%*") config-alist))))

	(propertize (format "%s" (apply (cadr result) (cddr result))) 'face `(:family ,(funcall (car result)) :inherit ))))

(defun powerline-ayman-window-numbering ()
  "Show window numbering icon."
  (propertize (format "%c" (+ (window-numbering-get-number) 9311))
			  'face `(:height 1.0 :inherit)
			  'display '(raise -0.0)))

(defun powerline-ayman-flycheck-count ()
  "Counts amount of errors/warnings in flycheck."
  (let* ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                  (+ (or .warning 0) (or .error 0)))))
    (if flycheck-current-errors
		(concat (all-the-icons-faicon "ban") (format " %s" count))
	  "")))

(defun powerline-ayman-flycheck-status ()
  "Show flycheck errors and if it's running."
    (let* ((text (cl-case flycheck-last-status-change
                 (finished    (powerline-ayman-flycheck-count))
                 (running     (all-the-icons-faicon "refresh"))
				 (no-checker  "")
                 (not-checked "")
                 (errored     "")
                 (interrupted "")))
         (face (cond
                (t `(:height 0.9 :inherit)))))

	  (propertize text
				  'face face
				  'display '(raise 0.1)
				  'help-echo "Show Flycheck Errors"
				  'mouse-face 'mode-line-highlight
				  'local-map (make-mode-line-mouse-map 'mouse-1 'flycheck-list-errors))))

(defun powerline-ayman-major-mode ()
  "Show major mode icon instead of name if in GUI."
  (let ((icon (all-the-icons-icon-for-buffer)))
	(unless (symbolp icon) ;; This implies it's the major mode
	  (propertize icon
				  'help-echo (format "Major-mode: `%s`" major-mode)
				  'display '(raise -0.1)
				  'face `(:height 1.0 :family ,(all-the-icons-icon-family-for-mode major-mode) :inherit)))))

(defun powerline-ayman-theme ()
  "Custom powerline theme."
  (interactive)
  (setq-default mode-line-format
				'("%e"
				  (:eval
				   (let* (
						  (active (powerline-selected-window-active))
						  (mode-line (if active 'mode-line 'mode-line-inactive))
						  (face0 (if active 'ayman-powerline-active0 'powerline-inactive0))
						  (face1 (if active 'ayman-powerline-active1 'powerline-inactive1))
						  (face2 (if active 'ayman-powerline-active2 'powerline-inactive2))
						  
						  (separator-left
						   (intern
							(format "powerline-%s-%s"
									(powerline-current-separator)
									(car powerline-default-separator-dir))))
                          (separator-right
						   (intern
							(format "powerline-%s-%s"
									(powerline-current-separator)
									(cdr powerline-default-separator-dir))))
						  
						  (lhs (list
								(powerline-raw (powerline-ayman-modified) face0 'l)
								(if (and (> (count-windows) 1) (bound-and-true-p window-numbering-mode))
									(powerline-raw (powerline-ayman-window-numbering) face0 'l))
								(powerline-raw " " face0)
								(funcall separator-left face0 face1)
								(powerline-raw " " face1)
								(powerline-buffer-id face1 'l)
								(powerline-raw " " face1)
								(powerline-narrow face1 'l)
								(funcall separator-left face1 face2)))
						  
						  (rhs (list
								(if (bound-and-true-p flycheck-mode)
									(powerline-raw (powerline-ayman-flycheck-status) face2))
								(powerline-raw " " face2)
								(powerline-raw global-mode-string face2 'r)
								(funcall separator-right face2 face1)
								(powerline-raw " " face1)
								(powerline-major-mode face1 'r)
								(if (display-graphic-p)
									(powerline-raw (powerline-ayman-major-mode) face1)
								  )
								(powerline-raw " " face1)
								(funcall separator-right face1 face0)
								(powerline-raw " %p  " face0)
								(when powerline-display-hud
								  (powerline-hud face1 face0))
								(powerline-fill face2 0))))
					 
					 (concat (powerline-render lhs)
							 (powerline-fill face2 (powerline-width rhs))
							 (powerline-render rhs)))))))



;;; Packages
(use-package auto-complete ;;autocompletion on tab
  :ensure t
  :diminish auto-complete-mode
  :init
  (progn
    (global-auto-complete-mode t))
  :bind (:map ac-completing-map
			  ("C-n" . ac-next)
			  ("C-p" . ac-previous))
  :config
  (progn
    (use-package auto-complete-config)

    (ac-config-default)

    (setq ac-delay 0.02)
    (setq ac-use-menu-map t)
    (setq ac-use-quick-help nil)
    (setq ac-ignore-case nil)
    (setq ac-dwim t)
    (setq ac-use-fuzzy t)))

(use-package fuzzy ;;fuzzy autocompletion
  :ensure t)

(use-package ac-c-headers ;;c autocompletion
  :defer
  :ensure t
  :config
  (add-hook 'c-mode-hook
			(lambda ()
			  (add-to-list 'ac-sources 'ac-source-c-headers)
			  (add-to-list 'ac-sources 'ac-source-c-header-symbols t)
			  ))
  )

(use-package jedi ;;python autocompletion
  :defer
  :ensure t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup)
  )

(use-package web-mode ;;html autocompletion
  :defer
  :ensure t
  :config
  (progn
	(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	(setq web-mode-engines-alist
		  '(("django" . "\\.html\\'")))
	(setq web-mode-ac-sources-alist
		  '(("css" . (ac-source-css-property))
			("html" . (ac-source-words-in-buffer ac-source-abbrev))))
	(setq web-mode-enable-auto-closing t)
	(setq web-mode-enable-current-element-highlight t)
	(setq web-mode-style-padding 4)
	(setq web-mode-script-padding 4)
	(setq web-mode-css-indent-offset 4)
	(setq web-mode-code-indent-offset 4)
	)
  )

;;emmet for css and html completion
;;(use-package ac-emmet
;;  :ensure t)

(use-package rjsx-mode ;;better javascript + jsx
  :defer
  :ensure t)

(use-package smartparens ;;matching parentheses
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode)
	;;can't deleting matching delimiters
	;;(smartparens-strict-mode 1)
	(setq-default sp-highlight-pair-overlay nil)
	;;enter after bracket
	(defun my-create-newline-and-enter-sexp (&rest _ignored)
	  "Open a new brace or bracket expression, with relevant newlines and indent. "
	  (newline)
	  (indent-according-to-mode)
	  (forward-line -1)
	  (indent-according-to-mode))
	(sp-pair "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
	))

(use-package nlinum ;;line numbers
  :ensure t
  :config
  (progn
    (global-nlinum-mode 1)
    ;;linum background color
    (unless (display-graphic-p)
      (set-face-attribute 'linum nil :background "222")))
  )

(use-package swiper ;;better search
  :ensure t
  :diminish ivy-mode
  :bind
  (("\C-s" . swiper)
   ("\C-r" . swiper))
  )

(use-package rainbow-delimiters ;;rainbow parens!
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

(use-package multiple-cursors ;;multiple cursors
  :ensure t
  :bind (("C-." . mc/mark-next-like-this))
  )

(use-package flycheck ;;error checking
  :ensure t
  :diminish flycheck-mode
  :init
  (global-flycheck-mode)
  :config
  (progn
	;;colors of underlines
	(set-face-attribute 'flycheck-warning nil
						:underline "#FFF44F")
	(set-face-attribute 'flycheck-error nil
						:underline "#EE204D")
	;;no fringe arrows
	(setq flycheck-indication-mode nil))
  )

(use-package sr-speedbar ;;speedbar for file navigation
  :defer
  :ensure t
  :config
  (progn
	(setq speedbar-show-unknown-files t) ;; show all files
	(setq speedbar-use-images nil) ;; use text for buttons
	(setq sr-speedbar-auto-refresh nil))
  )

(use-package markdown-mode ;;markdown editing
  :defer
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  )

(use-package windresize ;;window resizing interactively
  :ensure t
  :bind (("C-c w" . windresize))
  )

(use-package ido ;;ido mode
  :ensure t
  :bind (("C-x b" . ido-switch-buffer))
  :config
  (ido-mode 1)
  (setq ido-ignore-buffers '("\\` " "^\*"))
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-case-fold t)
  (defun ido-define-keys ()
	"Make n and p work to move between ido match along with s, p."
	(define-key ido-completion-map (kbd "C-n") 'ido-next-match)
	(define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
  (add-hook 'ido-setup-hook 'ido-define-keys)
  )

(use-package ido-better-flex ;;ido flex (fuzzy)
  :ensure t
  :config
  (ido-better-flex/enable)
  )

(use-package ido-vertical-mode ;;ido vertically shown
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-show-count t)
  )

(use-package smex ;;ido for M-x
  :ensure t
  :bind (
		 ("M-x" . smex)
		 ("C-c M-x" . execute-extended-command))
  :config
  (smex-initialize)
  (defadvice smex (around space-inserts-hyphen activate compile)
	"Check if ido cannot complete command."
	(let ((ido-cannot-complete-command
		   `(lambda ()
			  (interactive)
			  (if (string= " " (this-command-keys))
				  (insert ?-)
				(funcall ,ido-cannot-complete-command)))))
	  ad-do-it))
  )

(use-package page-break-lines ;; Turn ^L into nice <hr>
  :ensure t
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode t)
  )

(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode))

(use-package all-the-icons ;;adding nice icon support to modeline
  :ensure t)

(use-package all-the-icons-dired ;;adding nice idon support to dired
  :ensure t
  :diminish all-the-icons-dired-mode
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package powerline ;;more aesthetic mode line, faster than spaceline
  :ensure t
  :init
  (powerline-ayman-theme)
  :config
  (setq powerline-height 18)
  (setq powerline-image-apple-rgb t)
  (if (display-graphic-p)
	  (setq powerline-default-separator 'slant)
	(setq powerline-default-separator 'utf-8)))

(use-package try ;;test out new packages
  :ensure t)

(use-package auctex ;;better latex editing
  :defer
  :ensure t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  (setq TeX-PDF-mode t))

(use-package latex-pretty-symbols ;;shows latex symbols on the fly in buffer
  :defer
  :ensure t)



;;; Dependencies
(use-package s
  :ensure t)
(use-package dash
  :ensure t)



;;; Local Packages
(byte-recompile-directory (expand-file-name "~/.emacs.d/lisp") 0) ;;byte compile local packages directory

(load "~/.emacs.d/lisp/f/f") ;;nice elisp file editing, used for java-compile-and-run

(load "~/.emacs.d/lisp/ido-speed-hack/ido-speed-hack") ;;ido-speed-hack to make M-x under smex snappy

(load "~/.emacs.d/lisp/emacs-strip/emacs-strip") ;;used to clean up screen for distraction free mode

(if (display-graphic-p)
	(progn
	  (load "~/.emacs.d/lisp/dashboard/dashboard") ;;dashboard for gui-emacs
	  (dashboard-setup-startup-hook)))



;;; Configs
(setq ns-use-srgb-colorspace t) ;;use full colorspace of mac display

(setq-default frame-title-format '("")) ;;frame title

(set-face-attribute 'default nil :font "Monaco") ;;default font

(setq-default cursor-type 'bar) ;;make cursor a line

(setq frame-resize-pixelwise t) ;;make frame sizable to pixels

;;default starting emacs size
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 80))

(setq ad-redefinition-action 'accept) ;;make ido-yes-no work with ad

(setq inhibit-startup-screen t) ;; get rid of startup screen
(setq initial-scratch-message "") ;; empty scratch message

;;get rid of scrollbar, toolbar, and menubar (in terminal) respectively
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(unless (display-graphic-p)
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)))

(setq default-directory "~") ;;set default starting directory

;;tab width to 4
(setq-default tab-width 4)
;;c, perl, python, and html indent to 4
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(set-variable 'python-indent-offset 4)
(setq sgml-basic-offset 4)

;;get rid of current selection when pasting
(delete-selection-mode 1)

(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))) ;;all backups go in one directory

(setq select-enable-clipboard t) ;;use os clipboard (windows & linux)

(show-paren-mode t) ;;highlight matching parentheses

(diminish 'abbrev-mode) ;;hide abbrev-mode
(diminish 'eldoc-mode) ;;hide eldoc

(if (display-graphic-p)
	(progn
	  (setq mouse-wheel-scroll-amount '(1 ((shift) .1) ((control) . nil)))
	  (setq mouse-wheel-progressive-speed nil))
  (progn
	(global-set-key (kbd "<mouse-4>") 'previous-line)
	(global-set-key (kbd "<mouse-5>") 'next-line)
	(setq mouse-wheel-scroll-amount '(1))
	(setq mouse-wheel-progressive-speed nil)
	(xterm-mouse-mode 1)
	(setq nlinum-format "%d ")))

;;prettify symbols
(global-prettify-symbols-mode +1)

;;make prettified symbols look regular on hover
(setq prettify-symbols-unprettify-at-point 'right-edge)

;;auctex find latex executables
(setq exec-path (append exec-path '("/Library/TeX/texbin")))


;;; Keybindings
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key [(meta shift up)]  'move-line-up)
(global-set-key [(meta shift down)]  'move-line-down)

(defun java-compile-and-run ()
  "Compile and run java files."
  (interactive)
  (shell-command-on-region
   ;;seperate buffer
   (point-min)
   (point-max)
   ;;da command
   (concat "javac " (buffer-file-name) " && " "java " (f-base buffer-file-name))
   ;;name of buffer
   "*java_compilation*"
   ))
(global-set-key (kbd "C-c j") 'java-compile-and-run)

(defun python-run ()
  "For running simple python files."
  (interactive)
  (shell-command-on-region
   ;;seperate buffr
   (point-min)
   (point-max)
   ;;running file
   (concat "python " (buffer-file-name))
   ;;buffr name
   "*python_run*"
   ))
(global-set-key (kbd "C-c p") 'python-run)

(defun c-compile-and-run ()
  "C compile and run."
  (interactive)
  (shell-command-on-region
   (point-min)
   (point-max)
   (concat "gcc " (buffer-file-name) " && " "./a.out"))
  "*c_compilation*"
  )
(global-set-key (kbd "C-c c") 'c-compile-and-run)

(defun open-html-file ()
  "Open current html file in default browser."
  (interactive)
  (shell-command-on-region
   (point-min)
   (point-max)
   (concat "open " (buffer-file-name))
   "*open*"))
(global-set-key (kbd "C-c h") 'open-html-file)

(defun close-and-kill-next-pane ()
  "If there are multiple windows, then close the other pane and kill the buffer in it also."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(defun flycheck-list-errors-toggle ()
  "Toggle listing flycheck errors buffer."
  (interactive)
  (if (get-buffer "*Flycheck errors*")
      (close-and-kill-next-pane)
    (flycheck-list-errors)))
(global-set-key (kbd "C-c f") 'flycheck-list-errors-toggle)

(defun copy-from-osx ()
  "Copy from OSX's GUI using pbcopy."
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  "TEXT: PUSH through pbpaste."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

;;copy and paste on osx, pbcopy & pbpaste
(if (eq system-type 'darwin)
    (progn
      (setq interprogram-cut-function 'paste-to-osx)
      (setq interprogram-paste-function 'copy-from-osx))
  )

(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))

(defun remove-scratch-buffer ()
  "Deletes the compile log buffer after start."
  (if (get-buffer "*Compile-Log*")
      (kill-buffer "*Compile-Log*")))
(add-hook 'after-init-hook 'remove-scratch-buffer)

(setq inhibit-startup-buffer-menu t) ;;Don't show *Buffer list* when opening multiple files at the same time.

(add-hook 'window-setup-hook 'delete-other-windows) ;;Show only one active window when opening multiple files at the same time.

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))

(defun run-speedbar ()
  "Run sr-speedbar and expand all."
  (interactive)
  (sr-speedbar-toggle))
(global-set-key (kbd "C-c s") 'run-speedbar)

(setq echo-keystrokes 0.01) ;;show keystrokes after 0.01 seconds

(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen) ;;fullscreen

;;don't copy word when you delete whole word
(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With ARG, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With ARG, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(defun my-delete-line ()
  "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (point)))
  (delete-char 1))

(defun my-delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))

										; bind them to emacs's default shortcut keys:
(global-set-key (kbd "C-S-k") 'my-delete-line-backward) ; Ctrl+Shift+k
(global-set-key (kbd "C-k") 'my-delete-line)
(global-set-key (kbd "M-d") 'my-delete-word)
(global-set-key (kbd "M-<DEL>") 'my-backward-delete-word)

(provide 'init)
;;; init.el ends here
