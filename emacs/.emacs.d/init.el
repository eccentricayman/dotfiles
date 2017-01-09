;;; init.el --- eccentricayman's emacs config

;;; Commentary:
;; C-c s : speedbar
;; C-c g : magit
;; C-c f : flycheck list errors
;; C-c j : Compile and run java file in current buffer
;; C-c p : Run python file in current buffer
;; C-c c : Compile and run C file in current buffer
;; C-c h : Open current HTML file in default browser (OSX Only)

;;; Code:

;;(let ((file-name-handler-alist nil))
;;(run-with-idle-timer
;;5 nil
;;(lambda ()
;;(setq gc-cons-threshold 1000000)
;;  (message "gc-cons-threshold restored to %S"
;;           gc-cons-threshold)))
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
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
    ("3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "962dacd99e5a99801ca7257f25be7be0cebc333ad07be97efd6ff59755e6148f" "64ca5a1381fa96cb86fd6c6b4d75b66dc9c4e0fc1288ee7d914ab8d2638e23a9" "0726d81a364bba24e311416c3c29674c7a09781ac13f16fe193f714c645b2bf4" "32c4ff8d6904594327c0c3cd9828c62673962d9e90b11fa3881fb330dd55c831" "e9101154806d5d9508b2c804b6a0cdc53236171a76a247350f822f1ec28b996c" "a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "b01e2d02a7bd9a67e8824bf1501f4fb9d5dce57941808f0af7020b47aaa9b294" "e3cf7c5e6fed398173ef56b6547bdcc2436604ecd3e60d46d51fb8d9a0a25ab0" "c2f49c919c31c7de1ace6f10eea91f64c6f2338a82a203eca2588e3447082e76" "01e067188b0b53325fc0a1c6e06643d7e52bc16b6653de2926a480861ad5aa78" "d6db7498e2615025c419364764d5e9b09438dfe25b044b44e1f336501acd4f5b" "721bb3cb432bb6be7c58be27d583814e9c56806c06b4077797074b009f322509" "06dbcfac3705aaaa79e1a3264c6fd44ef0cf86ef5ed67930e4007e63a8c1e8ee" "38f48e62e16e2c8f178c7e9de00aab382bc92d84ea382822907ed4e762388ae0" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "4ab95b35f7720043592b49d890003874aa1954a3cf299edde13657c6a9182d85" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "427fed191e7a766152e59ef0e2904283f436dbbe259b9ccc04989f3acde50a55" "d9dab332207600e49400d798ed05f38372ec32132b3f7d2ba697e59088021555" "41eb3fe4c6b80c7ad156a8c52e9dd6093e8856c7bbf2b92cc3a4108ceb385087" "3de3f36a398d2c8a4796360bfce1fa515292e9f76b655bb9a377289a6a80a132" "8cf1002c7f805360115700144c0031b9cfa4d03edc6a0f38718cef7b7cabe382" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "790e74b900c074ac8f64fa0b610ad05bcfece9be44e8f5340d2d94c1e47538de" "de0b7245463d92cba3362ec9fe0142f54d2bf929f971a8cdf33c0bf995250bcf" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "b61c55259c639a54628f91452b060b99c550a1269eb947e372321b806b68f114" "1160f5fc215738551fce39a67b2bcf312ed07ef3568d15d53c87baa4fd1f4d4e")))
 '(fci-rule-color "#3E4451")
 '(org-startup-truncated t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(package-selected-packages
   (quote
    (fortune-cookie highlight-indent-guides try page-break-lines smooth-scrolling geeknote markdown-mode sr-speedbar ac-emmet emmet-mode ac-c-headers auto-complete irony helm avy swiper nlinum-relative multiple-cursors windresize ido-better-flex ido-vertical-mode smex recentf-ext rainbow-delimiters popup highlight-parentheses fsm atom-one-dark-theme)))
 '(show-paren-mode t)
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

;;startup buffer
(setq inhibit-startup-screen t)

;;empty scratch message
(setq initial-scratch-message "")

;;emacs sizing
(setq initial-frame-alist '((top . 50) (left . 300) (width . 90) (height . 40)))

;;color theme
(load-theme 'atom-one-dark)

;;enable parens
;;(require 'highlight-parentheses)
;;(global-highlight-parentheses-mode 1)
;;(show-paren-mode 1)
(setq show-paren-delay 0)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco" :height 120))))
 '(button ((t (:inherit default))))
 '(company-scrollbar-bg ((t (:background "#3232ff"))))
 '(company-scrollbar-fg ((t (:background "#1919ff"))))
 '(company-tooltip ((t (:inherit default :background "#0a0aff"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(fringe ((t (:background "#282C34")))))

;;because ido-speed-hacks is terribly coded update: doesn't work on startup :(
;;(setq warning-minimum-level :emergency)

;; Preset width nlinum
;;(add-hook 'nlinum-mode-hook
;;          (lambda ()
;;            (setq nlinum--width
;;                  (length (number-to-string
;;                           (count-lines (point-min) (point-max)))))))

 ;;;;;;;;;;;; misc ;;;;;;;;;;;;;;;;;;;;;;;
;; enable clipboard
(setq select-enable-clipboard t)
;; autocomplete paired brackets
(electric-pair-mode 1)
;;(global-set-key (kbd "<C-tab>") 'other-window)
;; tramp mode for editing through ssh
;;following line windows only
;;(setq tramp-default-method "ssh")
;; show paren mode
;;(show-paren-mode 1)
;;disabled because emacsclient is weird with it
(global-nlinum-mode 1)
;; (add-to-list 'default-frame-alist '(left-fringe . 8))
;; (add-to-list 'default-frame-alist '(right-fringe . 0))
;; (face-spec-set 'fringe
;;   '((((class color) (background dark))
;;      :background "#353A43")))
;;(setq fringe-mode "no-fringes")
;; enable lines mode
;;(global-linum-mode 1)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
;;make paste get rid of highlighted text
(delete-selection-mode 1)
;;;;;;;;;;;; misc ;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;byebye menu;;;;;;;;;;;;;;;;;
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;better terminal line, faster
;; (if (display-graphic-p)
;;     (progn
;;       (require 'spaceline-config)
;;       (spaceline-emacs-theme)
;;       ;;this fixes the spaceline issues on os x, but makes emacs look uglier
;;       ;;deciding whether to comment the next line was the hardest decision
;;       ;;of my life
;;       ;;(setq ns-use-srgb-colorspace nil)
;;       (spaceline-compile)
;;       (global-hl-line-mode 1))
;;   (progn
;;     ;;(powerline-default-theme)
;;     (require 'airline-themes)
;;     (load-theme 'airline-distinguished)
;;     (global-set-key (kbd "<mouse-4>") 'previous-line)
;;     (global-set-key (kbd "<mouse-5>") 'next-line)
;;     (xterm-mouse-mode)))

(defun my-frame-config (frame)
  "Custom behaviours for new FRAME."
  (with-selected-frame frame
    (if (display-graphic-p)
        (progn
          ;;custom startscreen
          (use-package page-break-lines)
          (load "~/.emacs.d/lisp/dashboard/dashboard.elc")
          ;;(setq dashboard-items '((recents  . 10)))
          (dashboard-setup-startup-hook)
          (require 'spaceline-config)
          (if (fboundp 'menu-bar-mode) (menu-bar-mode 1))
          (setq ns-use-srgb-colorspace t)
          (setq powerline-default-separator 'utf-8)
          ;;bigger spaceline but worse colors
          ;;(setq powerline-height 18)
          ;;(setq ns-use-srgb-colorspace nil)
          (powerline-reset)
          (spaceline-emacs-theme)
          (spaceline-toggle-flycheck-error-off)
          (spaceline-toggle-flycheck-warning-off)
          (spaceline-compile)
          (setq nlinum-format "%d ")
          (set-fringe-mode 0)
          (global-hl-line-mode 0)
          ;;better smooth scrolling
          (use-package smooth-scrolling
            :ensure t
            :config (setq smooth-scroll-margin 2)
            :init (smooth-scrolling-mode 1))
          (setq mouse-wheel-scroll-amount '(1 ((shift) .1) ((control) . nil)))
          (setq mouse-wheel-progressive-speed nil)
          )
      (progn
        (use-package fortune-cookie
          :config
          (setq fortune-cookie-cowsay-args  "-s")
          (fortune-cookie-mode))
        (require 'airline-themes)
        (load-theme 'airline-distinguished)
        (setq airline-cursor-colors nil)
        (setq powerline-utf-8-separator-left        #xe0b0
              powerline-utf-8-separator-right       #xe0b2
              airline-utf-glyph-separator-left      #xe0b0
              airline-utf-glyph-separator-right     #xe0b2
              airline-utf-glyph-subseparator-left   #xe0b1
              airline-utf-glyph-subseparator-right  #xe0b3
              airline-utf-glyph-branch              #xe0a0
              airline-utf-glyph-readonly            #xe0a2
              airline-utf-glyph-linenumber          #xe0a1)
        (global-set-key (kbd "<mouse-4>") 'previous-line)
        (global-set-key (kbd "<mouse-5>") 'next-line)
        (set-fringe-mode 0)
        (setq mouse-wheel-scroll-amount '(1))
        (setq mouse-wheel-progressive-speed nil)
        (setq nlinum-format "%d ")
        (global-hl-line-mode 0)
        (set-face-background 'default "#222" (selected-frame))
        (xterm-mouse-mode)))))
;; run now
(my-frame-config (selected-frame))
;; and later
(add-hook 'after-make-frame-functions 'my-frame-config)

;;adios crappy terminal background
(defun on-after-init ()
  "Set background for terminal Emacs."
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "#222" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)

;;also useless, was to fix powerline messing up but we have spaceline now!
;;(add-hook 'desktop-after-read-hook 'powerline-reset)
;;;;;;;;;;;;;;;;;;;;;byebye menu;;;;;;;;;;;;;;;;;

;;;;;;;better mode-line ;;;;;;;;;;;;;;;;;;

;;;;;;;better mode-line;;;;;;;;;;;;;;;;;

;;;;;;;;starting dir;;;;;;;;;;;
(setq default-directory "~")

;;(setq desktop-buffers-not-to-save
;;    (concat "\\("
;;            "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^tags"
;;            "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
;;        "\\)$"))
;;(add-to-list 'desktop-modes-not-to-save 'dired-mode)
;;(add-to-list 'desktop-modes-not-to-save 'info-mode)
;;(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
;;(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
;;(defun emacs-process-p (pid)
;;  "if pid is the process id of an emacs process, return t, else nil.
;;also returns nil if pid is nil."
;;  (when pid
;;    (let ((attributes (process-attributes pid)) (cmd))
;;      (dolist (attr attributes)
;;        (if (string= "comm" (car attr))
;;            (setq cmd (cdr attr))))
;; q      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

;;(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
;;  "don't allow dead emacsen to own the desktop file."
;;  (when (not (emacs-process-p ad-return-value))
;;    (setq ad-return-value nil)))

;;(require 'sublimity)
;;(require 'sublimity-scroll)
;;(require 'sublimity-map)
;;(require 'sublimity-attractive)
;;(sublimity-mode 1)


;;grr @ difficult autocompletes.. (sike replaced with company mode)
;; (global-set-key [(tab)] 'smart-tab)

;; (defun smart-tab ()
;;   "this smart tab is minibuffer compliant: it acts as usual in
;;     the minibuffer. else, if mark is active, indents region. else if
;;     point is at the end of a symbol, expands it. else indents the
;;     current line."
;;   (interactive)
;;   (if (minibufferp)
;;       (unless (minibuffer-complete)
;;         (dabbrev-expand nil))
;;     (if mark-active
;;         (indent-region (region-beginning)
;;                        (region-end))
;;       (if (looking-at "\\_>")
;;           (dabbrev-expand nil)
;;         (indent-for-tab-command)))))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-width 4) ;; just in case
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq indent-line-function 'insert-tab)
;;change block to line cursor if enabled
;;(setq-default cursor-type 'bar)

;;(add-hook 'window-setup-hook 'on-after-init)
;; (require 'all-the-icons)
;; (require 'neotree)
;; (global-set-key (kbd "C-c f") 'neotree-toggle)
;; ;;fix neotree link color
;; (setq frame-background-mode 'dark)
;; ;;neotree icons
;; (setq neo-theme (if window-system 'icons 'arrow))

;;Magit
(global-set-key (kbd "C-c g") 'magit-status)

;;hides all the minor modes
(when (require 'diminish nil 'noerror)
  (eval-after-load "auto-complete"
    '(diminish 'auto-complete-mode))
  (eval-after-load "Irony"
    '(diminish 'irony-mode ""))
  (eval-after-load "abbrev"
    '(diminish 'abbrev-mode ""))
  (eval-after-load "yasnippet"
    '(diminish 'yas-minor-mode ""))
  (eval-after-load "flycheck"
    '(diminish 'flycheck-mode ""))
  (eval-after-load "ivy"
    '(diminish 'ivy-mode ""))
  ;;  (eval-after-load "emmet"
  ;;    '(diminish 'emmet-mode ""))
  )
;;can't find emmet
(diminish 'emmet-mode "")

;;random testing, makes cursor go through entire rainbow spectrum
;; (defvar blink-cursor-colors (list  "#ff0000" "#ff7f00" "#ffff00" "#00ff00" "0000ff" "551a8b" "4b0082"))
;; (setq blink-cursor-count 0)
;; (defun blink-cursor-timer-function ()
;;  (when (not (internal-show-cursor-p))
;;    (when (>= blink-cursor-count (length blink-cursor-colors))
;;      (setq blink-cursor-count 0))
;;    (set-cursor-color (nth blink-cursor-count blink-cursor-colors))
;;    (setq blink-cursor-count (+ 1 blink-cursor-count))
;;    )
;;  (internal-show-cursor nil (not (internal-show-cursor-p)))
;;  )
;;following is windows only for pdf viewing
;;(if (eq system-type 'windows-nt)
;;    (setq doc-view-ghostscript-program "gswin64c")
;;    (setq tramp-default-method "plink"))
(setq org-log-done 'time)
;;(defadvice doc-view-display (after fit-width activate)
;;  (doc-view-fit-width-to-window))
;;(setq fancy-about-text
(defun always-use-fancy-splash-screens-p ()
  "Just to make the always fancy splash screens work."
  1)
(defalias 'use-fancy-splash-screens-p 'always-use-fancy-splash-screens-p)
(global-set-key (kbd "M-x") 'smex)
;;(global-set-key (kbd "M-x") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(smex-initialize)
(defun smex-update-after-load (unused)
  "Update UNUSED commands in smex after loading."
  (when (boundp 'smex-cache)
    (smex-update)))
(add-hook 'after-load-functions 'smex-update-after-load)
(defadvice smex (around space-inserts-hyphen activate compile)
  "Check if ido cannot complete command."
  (let ((ido-cannot-complete-command
         `(lambda ()
            (interactive)
            (if (string= " " (this-command-keys))
                (insert ?-)
              (funcall ,ido-cannot-complete-command)))))
    ad-do-it))
(require 'ido-vertical-mode)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-vertical-mode 1)
(setq ido-vertical-show-count t)
(ido-better-flex/enable)
(defun ido-disable-line-truncation ()
  "No line truncation in ido."
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(defun ido-define-keys ()
  "Make n and p work to move between ido match along with s, p."
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)

;;ido ignore starred default buffers
(setq ido-ignore-buffers '("\\` " "^\*"))

;;non elpa/melpa/marmalade packages, like the m-x speed fixy thingy
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "~/.emacs.d/lisp/ido-speed-hack/ido-speed-hack.elc")

;;miscellanous new stuff!
;;(evil-define-key 'normal emacs-lisp-mode-map (kbd ";") 'smex)

;;moving lines up and down
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

(require 'swiper)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'swiper)
(define-key read-expression-map (kbd "\C-r") 'counsel-expression-history)

;;avy
(global-set-key (kbd "C-x SPC") 'avy-goto-char-2)
(setq avy-all-windows nil)

;;yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;;might slowdown, but allows vim + emacs
;;(require 'powerline)

;;;;;;temp;;;;;;
;;(spaceline-emacs-theme)
;;;;;;temp;;;;;;

;;;;;;;;;;fix this;;;;;;;;;;
;;dat vim
;;(defun to-evil ()
;;    (evil-mode 1)
;;    (powerline-default-theme)
;;    (load-theme 'airline-base16-shell-dark)
;;    )

;;(defun to-emacs ()
;;    (evil-mode 0)
;;    ;;(powerline-reset)
;;    (spaceline-emacs-theme))

;;defining keys for emacs-mode and vim-mode
;;(global-set-key (kbd "c-c e") 'to-emacs())
;;(global-set-key (kbd "c-c v") 'to-vim())

;;start in emacs mode
;;(add-hook 'after-init 'to-emacs())
;;;;;;;;;;fix this;;;;;;;;;;;
;; not needed in emacs 25
;(setq redisplay-dont-pause t)
;;)

;;\\no gnu emacs buffer when opening file
;; (when (and (string= "*scratch*" (buffer-name)) (not (buffer-file-name)))
;;   (progn
;;     (setq inhibit-startup-screen t)))

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)


;;flycheck 4 code check
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
;;crayola colors!
(set-face-attribute 'flycheck-warning nil
                    :underline "#FFF44F")
(set-face-attribute 'flycheck-error nil
                    :underline "#EE204D")
;;no sidelines
(setq flycheck-indication-mode nil)
;;flycheck error list
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

(setq-default flycheck-disabled-checkers '(javascript-eslint))

(global-set-key (kbd "C-c f") 'flycheck-list-errors-toggle)
;; (require 'company)

;; ;;(load "~/.emacs.d/lisp/company-complete-cycle/company-complete-cycle.elc")

;; ;;comapny tab completion
;; (add-hook 'after-init-hook 'global-company-mode)

;; ;;comapny then indent
;; (defun indent-or-complete ()
;;   (interactive)
;;   (if (looking-at "\\_>")
;;       (company-complete-common)
;;     (indent-according-to-mode)))

;; (global-set-key "\t" 'indent-or-complete)
;; (global-set-key "\t" ')

;; ;;dark background for company
;; (require 'color)

;; (let ((bg (face-attribute 'default :background)))
;;   (custom-set-faces
;;    `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
;;    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
;;    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
;;    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
;;    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; ;;eclim 4 java (hot damn this takes time to load)
;; (require 'company-emacs-eclim)
;; (company-emacs-eclim-setup)
;; (global-company-mode t)

;; ;;irony for c / c++
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony))

;; ;;jedi for python
;; (defun my/python-mode-hook ()
;;   (add-to-list 'company-backends 'company-jedi))

;; (add-hook 'python-mode-hook 'my/python-mode-hook)

;;using autocomplete-mode instead of company above
(require 'auto-complete)
(ac-config-default)
(ac-set-trigger-key "TAB")
(global-auto-complete-mode)

(define-key ac-completing-map "\C-n" 'ac-next)
(define-key ac-completing-map "\C-p" 'ac-previous)

;;c autocompletion
(require 'ac-c-headers)
(add-hook 'c-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-c-headers)
            (add-to-list 'ac-sources 'ac-source-c-header-symbols t)
            ;; (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/8.0.0/include")
            ;; (add-to-list 'achead:include-directories '"/usr/include")
            ))

;;irony stuff?
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(load "~/.emacs.d/lisp/ac-irony/ac-irony.elc")

(defun ac-irony-complete-tab ()
  "Complete tab in irony mode."
  (interactive)
  (if (looking-at "\\_>")
      (ac-complete-irony-async)
    (indent-according-to-mode)))
(defun my-ac-irony-setup ()
  "Enabling auto-complete, then irony-complete."
  (auto-complete-mode 1)
  (add-to-list 'ac-sources 'ac-source-irony)
  (define-key irony-mode-map (kbd "TAB") 'ac-irony-complete-tab)
  (ac-set-trigger-key "M-RET"))

(add-hook 'irony-mode-hook 'my-ac-irony-setup)

;;python autocompletion
(add-hook 'python-mode-hook 'jedi:ac-setup)

;;java autocompletion
;;(add-to-list 'load-path "~/.emacs.d/lisp/auto-java-complete/")
;;(require 'ajc-java-complete-config)
;;(add-hook 'java-mode-hook 'ajc-java-complete-mode)
;;(add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)

;; ;;autocomplete html
;; (defun setup-ac-for-html ()
;;   ;; Require ac-html since we are setup html auto completion
;;   (require 'ac-html)
;;   ;; Require default data provider if you want to use
;;   (require 'ac-html-default-data-provider)
;;   ;; Enable data providers,
;;   ;; currently only default data provider available
;;   (ac-html-enable-data-provider 'ac-html-default-data-provider)
;;   ;; Let ac-html do some setup
;;   (ac-html-setup)
;;   ;; Set your ac-source
;;   (setq ac-sources '(ac-source-html-tag
;;                      ac-source-html-attr
;;                      ac-source-html-attrv)))

;; ;;don't know why i need a seperate hook for autocomplete mode but it doesn't seem to work
;; (add-hook 'html-mode-hook 'auto-complete-mode)
;; (add-hook 'html-mode-hook 'setup-ac-for-html)

;;emmet, aka alternative autocomplete
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
;;emmet ac
(add-hook 'sgml-mode-hook 'ac-emmet-html-setup)
(add-hook 'css-mode-hook 'ac-emmet-css-setup)

;;processing
;; (autoload 'processing-mode "processing-mode" "processing mode" t)
;; (add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))

;; (setq processing-location "/home/eccentricayman/github/processing-3.1.1/processing-java")
;; (setq processing-application-dir "/home/eccentricayman/github/processing-3.1.1/processing")
;; (setq processing-sketchbook-dir "/home/eccentricayman/sketchbook")
;; (global-set-key (kbd "C-c P") 'processing-sketch-run)

;;loading f.el (os x is the load one)
;;(require 'f.el)
(load "~/.emacs.d/lisp/f/f.elc")

;;java sht (install f.el manually on os x)
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
   "*compile_and_run_java*"
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
   "*run_python*"
   ))

(global-set-key (kbd "C-c p") 'python-run)

(defun script-run ()
  "Run shell script."
  (interactive)
  (shell-command-on-region
   (point-min)
   (point-max)
   (concat ". " (buffer-file-name))
   "*run_script*"
   ))

(defun c-compile-and-run ()
  "C compile and run."
  (interactive)
  (shell-command-on-region
   (point-min)
   (point-max)
   (concat "gcc " (buffer-file-name) " && " "./a.out"))
  "*c_compile_and_run*"
  )

(global-set-key (kbd "C-c c") 'c-compile-and-run)

(defun open-html-file ()
  "Open current html file in default browser."
  (interactive)
  (shell-command-on-region
   (point-min)
   (point-max)
   (concat "open " (buffer-file-name))
   "*html_open*"))

(global-set-key (kbd "C-c h") 'open-html-file)

;;osx stuff
(if (eq system-type 'darwin)
    (progn
      (setq ns-function-modifier 'control)
      ;;(setq ns-command-modifier 'hyper)
      ;;(setq ns-command-modifier 'meta)
      (defvar *my-previous-buffer* t
        "can we switch?")

      (defun my-previous-buffer ()
        (interactive)
        (when *my-previous-buffer*
          (previous-buffer)
          (setq *my-previous-buffer* nil)
          (run-at-time "1 sec" nil (lambda ()
                                     (setq *my-previous-buffer* t)))))

      (defvar *my-next-buffer* t
        "can we switch?")

      (defun my-next-buffer ()
        (interactive)
        (when *my-next-buffer*
          (next-buffer)
          (setq *my-next-buffer* nil)
          (run-at-time "1 sec" nil (lambda ()
                                     (setq *my-next-buffer* t)))))

      (global-set-key [wheel-right] 'my-previous-buffer)
      (global-set-key [wheel-left] 'my-next-buffer)
      ;;so they don't give nil errors
      (setq frame-resize-pixelwise t)
      )
  )
(defun copy-from-osx ()
  "Copy from OSX's GUI using pbcopy."
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  "TEXT: PUSH through pbpaste."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(if (eq system-type 'darwin)
    (progn
      (setq interprogram-cut-function 'paste-to-osx)
      (setq interprogram-paste-function 'copy-from-osx))
  )

;;Removes *Completions* from buffer after you've opened a file.
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

;;Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;;Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

;;No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

;;stop asking for confirm on exiting with open processes
(require 'cl)
(require 'cl-lib)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))

;;speedbar stuff
(setq speedbar-show-unknown-files t) ;; show all files
(setq speedbar-use-images nil) ;; use text for buttons
(setq sr-speedbar-auto-refresh nil)

(defun speedbar-expand-all-lines ()
  "Expand all items in the speedbar buffer."
  (interactive)
  (goto-char (point-min))
  (while (not (eobp))
    (forward-line)
    (speedbar-expand-line)))

(defun run-speedbar ()
  "Run sr-speedbar and expand all."
  (interactive)
  (sr-speedbar-toggle))

(global-set-key (kbd "C-c s") 'run-speedbar)
;;use fzf sometimes cause it's faster
;;(global-set-key (kbd "C-c C-f") 'fzf)

;;windmove
(windmove-default-keybindings 'control)

;;markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;show keystrokes after 0.01 seconds
(setq echo-keystrokes 0.01)

(provide 'init)
;;; init.el ends here
