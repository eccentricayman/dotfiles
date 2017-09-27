;;; init.el --- eccentricayman's Emacs configuration

;;; Commentary:
;;; All backups in ~/.emacs.d/backups
;;; C-c w resizes window
;;; C-c s opens speedbar
;;; C-c-f Flycheck current buffer
;;; Run: C-c-j/h/p/c (java/browser/python/gcc)
;;; M-RET fullscreen

;;; Code:

;;more memory for garbage collection for init
(setq gc-cons-threshold 100000000)

;;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
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
	("a4c9e536d86666d4494ef7f43c84807162d9bd29b0dfd39bdf2c3d845dcc7b2e" "87d34869134b5497549a25dff75367d68aed7a8e3da598c9fa4e060a4e1f948e" "08b8807d23c290c840bbb14614a83878529359eaba1805618b3be7d61b0b0a32" "9b1c580339183a8661a84f5864a6c363260c80136bd20ac9f00d7e1d662e936a" "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "962dacd99e5a99801ca7257f25be7be0cebc333ad07be97efd6ff59755e6148f" "64ca5a1381fa96cb86fd6c6b4d75b66dc9c4e0fc1288ee7d914ab8d2638e23a9" "0726d81a364bba24e311416c3c29674c7a09781ac13f16fe193f714c645b2bf4" "32c4ff8d6904594327c0c3cd9828c62673962d9e90b11fa3881fb330dd55c831" "e9101154806d5d9508b2c804b6a0cdc53236171a76a247350f822f1ec28b996c" "a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "b01e2d02a7bd9a67e8824bf1501f4fb9d5dce57941808f0af7020b47aaa9b294" "e3cf7c5e6fed398173ef56b6547bdcc2436604ecd3e60d46d51fb8d9a0a25ab0" "c2f49c919c31c7de1ace6f10eea91f64c6f2338a82a203eca2588e3447082e76" "01e067188b0b53325fc0a1c6e06643d7e52bc16b6653de2926a480861ad5aa78" "d6db7498e2615025c419364764d5e9b09438dfe25b044b44e1f336501acd4f5b" "721bb3cb432bb6be7c58be27d583814e9c56806c06b4077797074b009f322509" "06dbcfac3705aaaa79e1a3264c6fd44ef0cf86ef5ed67930e4007e63a8c1e8ee" "38f48e62e16e2c8f178c7e9de00aab382bc92d84ea382822907ed4e762388ae0" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "4ab95b35f7720043592b49d890003874aa1954a3cf299edde13657c6a9182d85" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "427fed191e7a766152e59ef0e2904283f436dbbe259b9ccc04989f3acde50a55" "d9dab332207600e49400d798ed05f38372ec32132b3f7d2ba697e59088021555" "41eb3fe4c6b80c7ad156a8c52e9dd6093e8856c7bbf2b92cc3a4108ceb385087" "3de3f36a398d2c8a4796360bfce1fa515292e9f76b655bb9a377289a6a80a132" "8cf1002c7f805360115700144c0031b9cfa4d03edc6a0f38718cef7b7cabe382" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "790e74b900c074ac8f64fa0b610ad05bcfece9be44e8f5340d2d94c1e47538de" "de0b7245463d92cba3362ec9fe0142f54d2bf929f971a8cdf33c0bf995250bcf" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "b61c55259c639a54628f91452b060b99c550a1269eb947e372321b806b68f114" "1160f5fc215738551fce39a67b2bcf312ed07ef3568d15d53c87baa4fd1f4d4e")))
 '(fci-rule-color "#3E4451")
 '(org-startup-truncated t)
 '(package-archives
   (quote
	(("gnu" . "http://elpa.gnu.org/packages/")
	 ("melpa" . "http://melpa.org/packages/")
	 ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(package-selected-packages
   (quote
	(rjsx-mode js2-mode swiper recentf-ext smooth-scrolling spaceline page-break-lines ido-better-flex smex ido-sort-mtime ido-vertical-mode ido-yes-or-no windresize markdown-mode sr-speedbar jedi ac-c-headers flycheck multiple-cursors rainbow-delimiters nlinum smartparens fuzzy dash s web-mode cl-lib cl-lib-highlight try auto-complete use-package)))
 '(show-paren-mode t)
 '(sublimity-mode t)
 '(tool-bar-mode nil)
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
 '(powerline-active2 ((t (:inherit mode-line :background "#282C34"))))
 '(web-mode-doctype-face ((t (:foreground "#C678DD"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#E06C75"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "Grey"))))
 '(web-mode-html-tag-face ((t (:foreground "#61AFEF")))))

;;use-package install
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)

;;Theme
(use-package atom-one-dark-theme
	     :ensure t)
(load-theme 'atom-one-dark)

;;this is for regular emacs background
(defun emacs-terminal-background ()
  "Set background for terminal Emacs."
  (unless (display-graphic-p)
	(progn
	  (set-face-background 'default "222" (selected-frame))
	  (custom-set-faces
	   '(linum ((t (:inherit (shadow default) :background "222"))))))))
(add-hook 'window-setup-hook 'emacs-terminal-background)

;;this is for emacsclient background
(defun emacs-background-frame-config (frame)
  "Set background for terminal Emacs FRAME."
  (with-selected-frame frame
	(unless (display-graphic-p)
	  (emacs-terminal-background))))
;; run now
(emacs-background-frame-config (selected-frame))
;; and later
(add-hook 'after-make-frame-functions 'emacs-background-frame-config)

;;change the middle of the spaceline
(if (display-graphic-p)
	(custom-set-faces
	 '(powerline-active2 ((t (:inherit mode-line :background "#282C34")))))
  (custom-set-faces
   '(powerline-active2 ((t (:inherit mode-line :background "222")))))
  )

;;hide the default gnu startup help message
(defun display-startup-echo-area-message ()
  "Hide default GNU startup help message."
  (message ""))

;;; Configs
(setq-default frame-title-format '("")) ;;frame title

(set-face-attribute 'default nil :font "Monaco") ;;default font

(setq-default cursor-type 'bar) ;;make cursor a line

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

(show-paren-mode 1) ;;highlight matching parentheses

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

;;windmove, control to move between windows
(windmove-default-keybindings 'control)

;;; Packages
;;autocompletion on tab
(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :init
  (progn
    (auto-complete-mode t))
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

;;fuzzy autocompletion
(use-package fuzzy
  :ensure t)

;;c autocompletion
(use-package ac-c-headers
  :ensure t
  :config
  (add-hook 'c-mode-hook
			(lambda ()
			  (add-to-list 'ac-sources 'ac-source-c-headers)
			  (add-to-list 'ac-sources 'ac-source-c-header-symbols t)
			  ))
  )

;;python autocompletion
(use-package jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup)
  )

;;html autocompletion
(use-package web-mode
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

;;better javascript + jsx
(use-package rjsx-mode
  :ensure t)

;;matching parentheses
(use-package smartparens
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

;;line numbers
(use-package nlinum
  :ensure t
  :config
  (progn
	(global-nlinum-mode 1))
  )

;;better search
(use-package swiper
  :ensure t
  :diminish ivy-mode
  :bind
  (("\C-s" . swiper)
   ("\C-r" . swiper))
  )

;;rainbow parens!
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

;;multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-." . mc/mark-next-like-this))
  )

;;error checking
(use-package flycheck
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

;;speedbar for file navigation
(use-package sr-speedbar
  :ensure t
  :config
  (progn
	(setq speedbar-show-unknown-files t) ;; show all files
	(setq speedbar-use-images nil) ;; use text for buttons
	(setq sr-speedbar-auto-refresh nil))
  )

;;markdown editing
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  )

;;window resizing interactively
(use-package windresize
  :ensure t
  :bind (("C-c w" . windresize))
  )

;;ido mode
(use-package ido
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

;;ido flex (fuzzy)
(use-package ido-better-flex
  :ensure t
  :config
  (ido-better-flex/enable)
  )

;;ido vertically shown
(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-show-count t)
  )

;;ido for M-x
(use-package smex
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

;; Turn ^L into nice <hr>
(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode t)
  )

;;better mode-line
(use-package spaceline
  :ensure t
  :init
  (require 'spaceline-config)
  :config
  (setq powerline-default-separator 'utf-8)
  (powerline-reset)
  (spaceline-toggle-line-column-off)
  (spaceline-toggle-major-mode-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-flycheck-warning-off)
  (spaceline-toggle-version-control-off)
  (spaceline-emacs-theme)
  (setq ns-use-srgb-colorspace t))

(if (display-graphic-p)
	;;smooth scrolling in gui
	(use-package smooth-scrolling
	  :ensure t
	  :init (smooth-scrolling-mode 1)
	  :diminish smooth-scrolling-mode
	  :config
	  (setq smooth-scroll-margin 1)
	  (smooth-scrolling-mode 1)))

;;test out new packages
(use-package try
  :ensure t)

;;; Local Packages
(byte-recompile-directory (expand-file-name "~/.emacs.d/lisp") 0) ;;byte compile local packages directory

;;dependencies
(use-package s
  :ensure t)
(use-package dash
  :ensure t)

;;nice elisp file editing, used for java-compile-and-run
(load "~/.emacs.d/lisp/f/f")

(load "~/.emacs.d/lisp/ido-speed-hack/ido-speed-hack") ;;ido-speed-hack to make M-x under smex snappy

(load "~/.emacs.d/lisp/emacs-strip/emacs-strip") ;;used to clean up screen for distraction free mode

(if (display-graphic-p)
	(progn
	  (load "~/.emacs.d/lisp/dashboard/dashboard") ;;dashboard for gui-emacs
	  (dashboard-setup-startup-hook)))

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

(defun distraction-free-mode ()
  "Make current buffer distraction free."
  (interactive)
  (if (bound-and-true-p nlinum-mode)
      (progn
        ;;(toggle-frame-fullscreen)
        (nlinum-mode -1)
        (hidden-mode-line-mode)
        (bzg-big-fringe-mode)
        (custom-set-faces
                '(fringe ((t (:background "white")))))
        (custom-set-faces
                '(default ((t (:background "black" :foreground "grey"))))
                '(fringe ((t (:background "black"))))))
      (revert-buffer t t)
      (bzg-big-fringe-mode -1)
      (if (display-graphic-p)
          (custom-set-faces
           '(default ((t (:background "#282C34"))))
           '(fringe ((t (:background "#282C34")))))
        (custom-set-faces
         '(default ((t (:background "292929"))))
         '(fringe ((t (:background "292929"))))))))
(global-set-key (kbd "C-c d") 'distraction-free-mode)

(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen) ;;fullscreen

;;splits window then finds file, takes time to put into workflow
;; (defun vsplit-last-buffer ()
;;   "Vertically split, and use last buffer."
;;   (interactive)
;;   (split-window-vertically)
;;   (other-window 1 nil)
;;   (ido-find-file)
;;   )
;; (defun hsplit-last-buffer ()
;;   "Horizontally split, and use last buffer."
;;   (interactive)
;;    (split-window-horizontally)
;;    (other-window 1 nil)
;;    (ido-find-file)
;;   )
;; (global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
;; (global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

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
