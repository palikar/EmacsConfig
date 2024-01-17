;; Speeding things up!
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

(setq vc-follow-symlinks t)
(setq debug-on-error 't)

;; Global variables
(defvar config-directory (file-name-directory user-init-file))

(add-to-list 'custom-theme-load-path (concat config-directory "lisp"))
(add-to-list 'load-path (concat config-directory "lisp"))

;; (require ' smooth-scroll)

(require 'hlsl-mode)
(require 'glsl-mode)
(require 'scroll-on-jump)
(require 'pixel-scroll)
(require 'dumb-jump)
(require 'zygospore)
(require 'ag)
(require 'ibuffer-vc)
(require 'typit)
(require 'git-timemachine)
(require 'editorconfig)
(require 'magit)
(require 'markdown-mode)
(require 'ace-window)
(require 'bind-key)
(require 'volatile-highlights)
(require 'smartparens)
(require 'wrap-region)
(require 'nlinum)
(require 'uniquify)
(require 'arnaud-framer)
(require 'hungry-delete)
(require 'crux)
(require 'google-this)
(require 'cmake-mode)
(require 'emmet-mode)
(require 'web-mode)
(require 'yasnippet)
(require 'aggressive-indent)
(require 'vsexp)
(require 'projectile)
(require 'helm-core)
(require 'helm-semantic)
(require 'helm)
(require 'helm-lib)
(require 'helm-config)
(require 'ibuffer)
(require 'imenu-list)
(require 'company)
(require 'iy-go-to-char)
(require 'iedit)
(require 'diminish)
(require 'gtags)
(require 'helm-projectile)
(require 'helm-ag)
(require 'drag-stuff)
(require 'ox-gfm)
(require 'anzu)
(require 'visible-mark)

;; (require 'org-bullets)
(global-visible-mark-mode 't)

(anzu-mode 't)

(editorconfig-mode 1)

(defvar indirect-mode-name nil
  "Mode to set for indirect buffers.")
(make-variable-buffer-local 'indirect-mode-name)
(defun indirect-region (start end)
  "Edit the current region in another buffer.
    If the buffer-local variable `indirect-mode-name' is not set, prompt
    for mode name to choose for the indirect buffer interactively.
    Otherwise, use the value of said variable as argument to a funcall."
  (interactive "r")
  (let ((buffer-name (generate-new-buffer-name "*indirect*"))
	(mode
	 (if (not indirect-mode-name)
	     (setq indirect-mode-name
		   (intern
		    (completing-read
		     "Mode: "
		     (mapcar (lambda (e)
			       (list (symbol-name e)))
			     (apropos-internal "-mode$" 'commandp))
		     nil t)))
	   indirect-mode-name)))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
    (funcall mode)
    (narrow-to-region start end)
    (goto-char (point-min))
    (shrink-window-if-larger-than-buffer)))

(defun duplicate-line-down()
  (interactive)
  (let ((saved-position (point)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (next-line 1)
    (yank)
    (goto-char saved-position)
    )
  )

(defun duplicate-line-up()
  (interactive)
  (let ((saved-position (point)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (move-beginning-of-line 1)
    (open-line 1)
    (yank)
    (goto-char saved-position)
    (next-line 1)
    )
  )

(defun smarter-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun ask-before-closing ()
  "Close only if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to close this frame? ")) (save-buffers-kill-emacs)
    (message "Canceled frame close")))

(defun transpose-windows (arg) ;; yes, I know, there is also a crux-function that does the exact same thing...still...!!!
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
	    (next-win (window-buffer (funcall selector))))
	(set-window-buffer (selected-window) next-win)
	(set-window-buffer (funcall selector) this-win)
	(select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(defun find-myinit-file ()
  "Open the myinit.org file which is my actual configuration file."
  (interactive)
  (find-file-other-window (concat config-directory "init.el")))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (save-excursion
    (let (beg end)
      (if (region-active-p)
	  (setq beg (region-beginning) end (region-end))
	(setq beg (line-beginning-position) end (line-end-position)))
      (comment-or-uncomment-region beg end))))

(defun kill-whole-word ()
  (interactive)
  (backward-word)
  (kill-word 1))

(defun kill-whole-line ()
  (interactive)
  (move-beginning-of-line 'nil)
  (kill-line))

(defun replace-or-delete-pair (open)
  "Replace pair at point by OPEN and its corresponding closing character.
The closing character is lookup in the syntax table or asked to
the user if not found."
  (interactive
   (list
    (read-char
     (format "Replacing pair %c%c by (or hit RET to delete pair):"
	     (char-after)
	     (save-excursion
	       (forward-sexp 1)
	       (char-before))))))
  (if (memq open '(?\n ?\r))
      (delete-pair)
    (let ((close (cdr (aref (syntax-table) open))))
      (when (not close)
	(setq close
	      (read-char
	       (format "Don't know how to close character %s (#%d) ; please provide a closing character: "
		       (single-key-description open 'no-angles)
		       open))))
      (replace-pair open close))))

(defun replace-pair (open close)
  "Replace pair at point by respective chars OPEN and CLOSE.
If CLOSE is nil, lookup the syntax table. If that fails, signal
an error."
  (let ((close (or close
		   (cdr-safe (aref (syntax-table) open))
		   (error "No matching closing char for character %s (#%d)"
			  (single-key-description open t)
			  open)))
	(parens-require-spaces))
    (insert-pair 1 open close))
  (delete-pair)
  (backward-char 1))

(defun open-dot-compile-file ()
  (interactive)
  (find-file (concat (file-name-as-directory (projectile-project-root)) ".compile")))

(defun joe/smooth-scroll-half-page-down ()
  "Smooth scroll down"
  (interactive)
  (let ((half-height (/ (window-height) 2)))
    (pixel-scroll-precision-interpolate (* 5 (- half-height)))))

(defun joe/smooth-scroll-half-page-up ()
  "Smooth scroll down"
  (interactive)
  (let ((half-height (/ (window-height) 2)))
    (pixel-scroll-precision-interpolate (* 5 half-height))))

(defun joe/smooth-scroll-line-up ()
  "Smooth scroll down"
  (interactive)
  (let ((half-height 4))
    (pixel-scroll-precision-interpolate (* 5 half-height))))

(defun joe/smooth-scroll-line-down()
  "Smooth scroll down"
  (interactive)
  (let ((half-height 4))
    (pixel-scroll-precision-interpolate (* 5 (- half-height)))))

(add-hook 'hlsl-mode-hook
	  (lambda ()
	    (setq tab-width 4)
	    (setq indent-tabs-mode t)
	    ))

(add-to-list 'auto-mode-alist '("\\.inc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hlsl\\'" . hlsl-mode))
(add-to-list 'auto-mode-alist '("\\.ihlsl\\'" . hlsl-mode))
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("CMakeLists.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.bui\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.api\\'" . web-mode))
(add-to-list 'auto-mode-alist '("/some/react/path/.*\\.js[x]?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))


(global-unset-key  ( kbd "<prior>"))
(global-unset-key  ( kbd "<next>"))
(global-unset-key  ( kbd "<home>"))
(global-unset-key  ( kbd "<end>"))
(global-unset-key  ( kbd "<insert>"))
(global-unset-key  ( kbd "<insert>"))
(global-unset-key  ( kbd "C-<home>"))
(global-unset-key  ( kbd "C-<end>"))

;; (setq scroll-on-jump-duration 0.2)
;; (setq scroll-on-jump-smooth 't)
;; (setq scroll-on-jump-curve 'smooth)

;; (scroll-on-jump-advice-add rsmooth-scroll)
;; (scroll-on-jump-advice-add undo)
;; (scroll-on-jump-advice-add pop-global-mark)
;; (scroll-on-jump-advice-add isearch-search)
;; (scroll-on-jump-advice-add helm-semantic-or-imenu)

;; (scroll-on-jump-advice-remove previous-line)
;; (scroll-on-jump-advice-remove next-line)
;; (scroll-on-jump-advice-remove forward-paragraph)
;; (scroll-on-jump-advice-remove backward-paragraph)

;; (global-set-key (kbd "C-l") (scroll-on-jump-interactive 'recenter))
;; (bind-key* "C-l" (scroll-on-jump-interactive 'recenter))

(bind-key* "<f12>" 'dumb-jump-go)

(bind-key* "C-x p" 'previous-buffer)

;; (bind-key* "C-x n" 'next-buffer)

(bind-key* "C-c	 m" 'global-visible-mark-mode)

(bind-key* "C-TAB" 'self-insert-command)

(bind-key* "C-<tab>" 'self-insert-command)

(bind-key* "M-<f2>" 'tabify)

(bind-key* "M-<f3>" 'untabify)

(bind-key* "M-c" 'syntax-subword-capitalize)

(bind-key* "<deletechar>" 'hungry-delete-forward)

(bind-key* "C-a" 'smarter-move-beginning-of-line)

(bind-key* "C-x C-c" 'ask-before-closing)

(bind-key* "C-<f1>" 'toggle-transparency)

(bind-key* "M-<f8>" 'fci-mode)

(bind-key* "C-<f9>" 'hide-mode-line-mode)

(bind-key* "<f10>" 'tool-bar-mode)

(bind-key* "C-<f10>" 'scroll-bar-mode)

(bind-key* "C-<f12>" 'nlinum-mode)

(bind-key* "M-n" 'forward-paragraph)

(bind-key* "M-p" 'backward-paragraph)

(bind-key* "<f5>" 'revert-buffer)

(bind-key* "C-<prior>" 'scroll-down-line)3

(bind-key* "C-<next>" 'scroll-up-line)

(bind-key* "C-c d" 'delete-file)

(bind-key* "C-S-<down>"  'duplicate-line-down)

(bind-key* "C-S-<up>"  'duplicate-line-up)

(bind-key* "C-+" 'text-scale-increase)

(bind-key* "C--" 'text-scale-decrease)

(bind-key* "C-z" 'zap-up-to-char)

(bind-key* "C-x r e" 'eval-region)

(bind-key* "<f5>" 'revert-buffer)

(bind-key* "M-j <f1>" 'customize-group)

(bind-key* "M-j <f2>" 'setup-packages)

(bind-key* "M-j <f3>" 'package-install)

(bind-key* "C-x k" 'kill-this-buffer)

(bind-key* "C-x K" 'kill-buffer)

(bind-key* "C-c w r" 'replace-or-delete-pair)

(bind-key* "C-c w w" 'kill-whole-word)

(bind-key* "C-c w l" 'kill-whole-line)

(bind-key* "M-<f1>" 'whitespace-cleanup)

(bind-key* "C-<Scroll_Lock>" 'list-installed-packages)

(bind-key* "C-<f7>" 'toggle-transparency)

(bind-key* "M-+" 'backward-paragraph)

(bind-key* "M-#" 'forward-paragraph)

(bind-key* "M-^" 'join-line)

(bind-key* "C-x 4 t" 'transpose-windows)

(bind-key* "C-c <left>"  'windmove-left)

(bind-key* "C-c <right>" 'windmove-right)

(bind-key* "C-c <up>"    'windmove-up)

(bind-key* "C-c <down>"  'windmove-down)

(setq aw-ignore-current t)

(bind-key* "C-x o" 'ace-window)

(bind-key* "C-x M-o" 'other-frame)

(bind-key* "C-c o" 'crux-open-with)

(bind-key* "C-c r" 'crux-rename-file-and-buffer)

(bind-key* "C-c i" 'find-myinit-file)

(bind-key* "C-c I" 'crux-find-user-init-file)

(bind-key* "C-c c" 'open-dot-compile-file)

(bind-key* "C-c 1" 'crux-create-scratch-buffer)

(bind-key* "C-c S" 'crux-find-shell-init-file)

(bind-key* "M-k" 'crux-kill-line-backwards)

(bind-key* "C-c t" 'crux-visit-term-buffer)

(bind-key* "C-c g" 'google-this-mode-submap)

(bind-key* "C-c g c" 'google-this-cpp-reference)

(bind-key* "C-c g z " 'zeal-at-point)

(bind-key* "C-c C-x s" 'org-attach-screenshot org-mode-map)

(bind-key* "M-S-<down>" 'org-move-subtree-down org-mode-map)

(bind-key* "M-S-<up>" 'org-move-subtree-up org-mode-map)

(bind-key* "C-c n" 'org-table-insert-row org-mode-map)

(bind-key* "C-c v" 'org-table-insert-column org-mode-map)

(bind-key* "C-/" 'comment-or-uncomment-region-or-line)

(bind-key* "M-ü" 'hs-show-all)

(bind-key* "C-M-ü" 'hs-hide-all)

(bind-key* "C-ü" 'hs-toggle-hiding)

(bind-key* "C-c y n"  'yas/new-snippet)

(bind-key* "C-c y v"  'yas/visit-snippet-file)

(bind-key* "C-c y r"  'yas/reload-all)

(bind-key* "M-j j b" 'json-pretty-print-buffer)

(bind-key* "M-j j r" 'json-pretty-print)

(bind-key* "C-M-y" 'sp-backward-up-sexp)

(bind-key* "C-M-x" 'sp-up-sexp)

(bind-key* "C-M-SPC" 'vsexp-mark-sexp)

(bind-key* "C-M-k" 'vsexp-kill-sexp)

(bind-key* "C-M-S-SPC" 'vsexp-mark-sexp-whole)

(bind-key* "C-M-S-k" 'vsexp-kill-sexp-whole)

(bind-key* "C-M-w" 'vsexp-kill-save-sexp)

(bind-key* "C-M-S-w" 'vsexp-kill-save-sexp-whole)

(bind-key* "C-c w i" 'vsexp-mark-inside)

(bind-key* "C-c w o" 'vsexp-mark-outside)

(bind-key* "C-x C-f" 'helm-find-files)

(bind-key* "M-x" 'helm-M-x)

(bind-key* "C-x b" 'helm-mini)

(bind-key* "C-c b" 'helm-semantic-or-imenu)

(bind-key* "M-s" 'helm-projectile-ag)

(bind-key* "C-x c C-a" 'helm-apt)

(bind-key* "C-x c M-m" 'helm-complete-file-name-at-point)

(bind-key* "C-x c C-s" 'helm-occur-from-isearch)

(bind-key* "C-x r h" 'helm-register)

(bind-key* "C-x c k" 'helm-execute-kmacro)

(bind-key* "M-y" 'helm-show-kill-ring)

(bind-key* "M-m" 'company-complete)

(bind-key* "C-c f" 'iy-go-up-to-char)

(bind-key* "C-c F" 'iy-go-up-to-char-backward)

(bind-key* "M-i"   'iedit-mode)

(bind-key* "C-x C-b" 'ibuffer)

(bind-key* "M-<up>" 'drag-stuff-up)

(bind-key* "M-<down>" 'drag-stuff-down)

(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

(setq inhibit-startup-message t)
(setq frame-title-format '("Emacs " emacs-version))
(setq cursor-type 'box)
(setq visible-bell 'nil)

;; Interface tweaks
(tool-bar-mode -1)
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(fringe-mode '(0 . 0))
(global-nlinum-mode -1)
(global-visual-line-mode 1)
(global-hl-line-mode 1)
(global-prettify-symbols-mode +1)
(scroll-bar-mode 0)
(set-fill-column 80)
(face-spec-set 'secondary-selection '((t (:background "light sky blue" :foreground "black"))))


(menu-bar-mode -1) ;; not menu bar please!

;; Behaviour tweaks
(drag-stuff-global-mode)
(setq indent-tabs-mode nil)
(setq auto-save-default nil)
(setq backup-inhibited t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-follow-mouse nil)
(setq scroll-step 1) ;;smooth-ish scrolling
(setq confirm-kill-emacs 'y-or-n-p) ;; Sometimes I fat finger C-x C-c
(setq save-interprogram-paste-before-kill t)
(setq auto-revert-verbose nil) ;; everything is seemless
(setq vc-follow-symlinks t) ;; it asks you everytime otherwise
(delete-selection-mode 1) ;; it's really weird working without that
(load (concat config-directory "lisp/syntax-subword"))
(global-syntax-subword-mode 1) ;; easy workings with camel case, snake case and pretty much anything else
(global-auto-revert-mode 1) ;; see changes on disc as quick as possible
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8-dos)
(set-terminal-coding-system 'utf-8-dos)
(set-keyboard-coding-system 'utf-8-dos)
(setq-default buffer-file-coding-system 'utf-8-dos)
(set-buffer-file-coding-system 'dos)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq tab-always-indent 'complete)
(setq require-final-newline t)
(setq mouse-yank-at-point t)
(setq create-lockfiles nil)
(auto-compression-mode t)

(volatile-highlights-mode t)

(recentf-mode t)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup 'never)

(add-to-list 'recentf-exclude "\\.windows\\'")
(add-to-list 'recentf-exclude "\\.revive\\'")
(add-to-list 'recentf-exclude "\\/ssh:\\'")


(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(wrap-region-add-wrapper "=" "=")
(wrap-region-add-wrapper "/" "/")
(wrap-region-add-wrapper "_" "_")
(wrap-region-add-wrapper "+" "+")
(wrap-region-add-wrapper "*" "*")
(wrap-region-add-wrapper "~" "~")
(wrap-region-add-wrapper "$" "$")
(wrap-region-add-wrapper "<" ">")
(wrap-region-add-wrapper ">" "<")
(wrap-region-global-mode t)

(smartparens-global-mode 1)


(setq scroll-margin 7)
(setq scroll-conservatively 0)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)

(setq-default scroll-up-aggressively 0.01
	      scroll-down-aggressively 0.01)

(scroll-all-mode -1)
(setq scroll-conservatively most-positive-fixnum)
(setq scroll-preserve-screen-position t)

(setq custom-file (concat config-directory "custom.el"))
(load custom-file)

(global-framer-mode 't)

(load (concat config-directory "lisp/spacemacs-common.el"))
(setq custom-enabled-themes (quote (spacemacs-dark)))
(setq custom-safe-themes t)
(load-theme 'spacemacs-dark)


(global-hungry-delete-mode)

(google-this-mode 1)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(setq org-support-shift-select (quote always))
(setq org-startup-indented t)
(setq org-hide-leading-stars t)
(setq org-babel-python-command "python")
(setq org-export-html-postamble nil)
(setq org-startup-folded (quote overview))
(setq org-log-done 'time)

(setq org-pretty-entities t)
(setq org-export-babel-evaluate nil)
(setq org-export-with-smart-quotes t)
(setq org-enable-priority-commands nil)
(setq org-html-htmlize-output-type 'css)

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-edit-src-content-indentation 0)

(setq  org-replace-disputed-keys 't)

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(add-hook 'proge-mode-hook 'semmantic-highlight-func-mode)
(show-paren-mode 1)

(set-default 'semantic-case-fold t)

(set-default 'semantic-case-fold t)

(add-hook 'prog-mode-hook 'hs-minor-mode)

(setq c-default-style "linux")
(setq c-basic-offset 4)
(setq tab-width 4)
(setq indent-tabs-mode t)
(setq c-noise-macro-names '("constexpr"))

(defun vlad-cc-style()
  (c-set-style "linux")
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (c-set-offset 'innamespace '0)
  (c-set-offset 'inextern-lang '0)
  (c-set-offset 'inline-open '0)
  (c-set-offset 'label '*)
  (c-set-offset 'comment-intro 0)
  (c-set-offset 'arglist-intro 'c-lineup-arglist-intro-after-paren)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'case-label '*)
  (c-set-offset 'access-label '/)
  (c-set-offset 'inlambda 0)
  (c-set-offset 'arglist-cont-nonempty '++)
  (c-set-offset 'lambda-intro-cont 0)
  (c-set-offset 'brace-list-open 0)
  (c-set-offset 'brace-list-close 0)
  (c-set-offset 'brace-list-intro '+))

(add-hook 'c++-mode-hook 'vlad-cc-style)
(add-hook 'c++-mode-hook (lambda () (setq sp-escape-quotes-after-insert nil)))


(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

(setq cmake-tab-width 4)



(defun my-web-mode-hook ()

  (emmet-mode 1)

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (setq web-mode-block-padding 0)
  (setq web-mode-markup-indent-offset 2)

  (setq web-mode-extra-auto-pairs '(("erb"  . (("beg" "end")))
				    ("php"  . (("beg" "end")
					       ("beg" "end")))))

  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-current-column-highlight t)

  (setq web-mode-ac-sources-alist '(("css" . (ac-source-css-property))
				    ("html" . (ac-source-words-in-buffer ac-source-abbrev)))))
(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq yas-snippet-dirs '())

(setq yas-snippet-dirs `(,(concat user-emacs-directory "snippets")))

(yas-global-mode 1)
(setq helm-yas-space-match-any-greedy t)
(setq yas-triggers-in-field t)

;; (global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)
(add-to-list
 'aggressive-indent-dont-indent-if
 '(and (derived-mode-p 'c++-mode)
       (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
			   (thing-at-point 'line)))))

(defun aggressive-indent--indent-if-changed (buffer)
  "Indent any region that changed in BUFFER in the last command loop."
  (if (not (buffer-live-p buffer))
      (and aggressive-indent--idle-timer
	   (cancel-timer aggressive-indent--idle-timer))
    (with-current-buffer buffer
      (when (and aggressive-indent-mode aggressive-indent--changed-list)
	(save-excursion
	  (save-selected-window
	    (aggressive-indent--while-no-input
	      (aggressive-indent--proccess-changed-list-and-indent))))
	(when (timerp aggressive-indent--idle-timer)
	  (cancel-timer aggressive-indent--idle-timer))))))


(setq projectile-completion-system 'helm)
(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(projectile-mode)

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)

(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)


(setq helm-M-x-fuzzy-match t)

(setq helm-exit-idle-delay 0)
(setq helm-ag-fuzzy-match t)
(setq helm-ag-command-option "--cpp -U")

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 50)
(setq helm-buffer-max-length 50)

(helm-mode 1)

(defclass tohiko/helm-source-file-buffers (helm-source-buffers)
  ((candidate-transformer :initform (lambda (buffers)
									  (cl-loop for buf in buffers
											   when (with-current-buffer
														buf buffer-file-name)
											   collect buf)))))

(defclass tohiko/helm-source-nonfile-buffers (helm-source-buffers)
  ((candidate-transformer :initform (lambda (buffers)
				      (cl-loop for buf in buffers
					       unless (with-current-buffer
							  buf buffer-file-name)
					       collect buf)))))

(setq tohiko/helm-source-file-buffers-list
      (helm-make-source "File Buffers" 'tohiko/helm-source-file-buffers))

(setq tohiko/helm-source-nonfile-buffers-list
      (helm-make-source "Non-file Buffers" 'tohiko/helm-source-nonfile-buffers))

(setq helm-mini-default-sources '(tohiko/helm-source-file-buffers-list
				  tohiko/helm-source-nonfile-buffers-list
				  helm-source-recentf
				  helm-source-buffer-not-found))

(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)

(setq ibuffer-saved-filter-groups
	'(("home"
	   ("Emacs-config" (or (filename . ".emacs")
			       (filename . "myinit.org")))
	   ("Org" (or (mode . org-mode)
		      (filename . "OrgMode")))
	   ("C++"
	    (or
	     (mode . c-mode)
	     (mode . c++-mode)
	     ))
	   ("Go Lang"
	    (mode . go-mode))
	   ("Python"
	    (mode . python-mode)
	    )
	   ("Configurations"
	    (or
	     (mode . conf-mode)
	     (mode . conf-space-mode)
	     (name . ".json")
	     (name . ".yaml")
	     (name . ".yml")
	     ))
	   ("Source Code"
	    (or
	     (mode . emacs-lisp-mode)
	     (mode . shell-script-mode)
	     (mode . json-mode)
	     ))
	   ("Dired"
	    (mode . dired-mode))
	   ("Scripts"
	    (name . ".sh")
	    )
	   ("Shaders"
	    (or
	     (name . ".hlsl")
	     (name . ".ihlsl")
	     )
	    )
	   ("Documents"
	    (mode . doctex-mode)
	    )
	   ("LaTeX"
	    (or
	     (mode . tex-mode)
	     (mode . latex-mode)
	     (name . ".tex")
	     (name . ".bib")))
	   ("Text" (name . ".txt"))
	   ("JS"
	    (or (mode . "JavaScript")
		(name . ".js")
		(mode . javascript-mode)))
	   ("Web Dev" (or (mode . html-mode)
			  (mode . css-mode)
			  (mode . webmode-mode)))
	   ("Emacs-created"
	    (or
	     (name . "^\\*")))
	   )))

(defun ibuffer-default ()
  (interactive)
  (ibuffer-switch-to-saved-filter-groups "home"))

;; dont call this here!
;; (ibuffer-default)

(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))


(setq ibuffer-formats
      '((mark modified read-only " "
	      (name 18 18 :left :elide)
	      " "
	      (size-h 9 -1 :right)
	      " "
	      (mode 16 16 :left :elide)
	      " "
	      filename-and-process)))


(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-auto-mode 1)
	     (ibuffer-switch-to-saved-filter-groups "home")))



(setq imenu-list-auto-resize t)
(setq imenu-list-after-jump-hook nil)


(setq company-minimum-prefix-length 3
      company-tooltip-align-annotations nil
      company-tooltip-flip-when-above nil
      company-idle-delay 'nil
      company-show-numbers nil
      company-echo-truncate-lines nil
      company-tooltip-maximum-width 100
      company-tooltip-minimum-width 100)
(global-company-mode t)


(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)

(setq company-backends '())
(setq company-frontends '(company-pseudo-tooltip-unless-just-one-frontend))
(setq company-tooltip-maximum-width 100)
(setq company-tooltip-minimum-width 100)

(face-spec-set 'company-preview '((t (:background "#444444" :foreground "light sky blue"))))
(face-spec-set 'company-tooltip '((t (:background "#444444" :foreground "light sky blue"))))
(face-spec-set 'company-tooltip-annotation '((t (:foreground "deep sky blue"))))

(defvar basic-company-backends '(company-files
				 company-capf
				 company-dabbrev-code
				 company-keywords
				 company-dabbrev))

(setq company-backends `(,basic-company-backends))


(face-spec-set 'iedit-occurrence '((t (:background "pale green" :foreground "black"))))

(defun diminish-em-all ()
  "docstring"
  (interactive)
  (diminish 'smartparens-mode)
  (diminish 'wrap-region-mode)
  (diminish 'super-save-mode)
  (diminish 'volatile-highlights-mode)
  (diminish 'isearch-mode)
  (diminish 'yas-minor-mode)
  (diminish 'google-this-mode)
  (diminish 'wg-mode)
  (diminish 'workgroups-mode)
  (diminish 'drag-stuff-mode)
  (diminish 'flyspell-mode)
  (diminish 'helm-mode)
  (diminish 'eldoc-mode)
  (diminish 'global-framer-mode)
  (diminish 'framer-mode)
  (diminish 'anzu-mode)
  (diminish 'company-mode)
  (diminish 'beacon-mode)
  (diminish 'flycheck-mode)
  (diminish 'hungry-delete-mode)
  (diminish 'org-indent-mode)
  (diminish 'hs-minor-mode)
  (diminish 'which-key-mode)
  (diminish 'iedit-mode)
  (diminish 'visual-line-mode)
  (diminish 'hs-minor-mode)
  (diminish 'aggressive-indent-mode)
  (diminish 'org-indent-mode)
  (diminish 'irony-mode)
  (diminish 'function-args-mode)
  (diminish 'abbrev-mode)
  (diminish 'sphinx-doc-mode)
  (diminish 'org-indent-mode)
  (diminish 'ycmd-mode)
  (diminish 'modern-c++-font-lock-mode)
  (diminish 'doxymacs-mode)
  (message "The modes should be away now!"))

(diminish-em-all)

(setq debug-on-error 'nil)

(add-hook 'python-mode-hook
	  (lambda () (setq indent-tabs-mode 'nil)))

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(setq require-final-newline 't)

(setq scroll-conservatively 10)
(setq scroll-margin 7)

(blink-cursor-mode 0)

(defun post-load-stuff ()
  (interactive)
  (set-cursor-color "#87cefa"))

(add-hook 'window-setup-hook 'post-load-stuff t)


(defun async-shell-command-no-window
    (command)
  (interactive)
  (let
      ((display-buffer-alist
	(list
	 (cons
	  "\\*Async Shell Command\\*.*"
	  (cons #'display-buffer-no-window nil)))))
    (async-shell-command
     command)))

(defun open-file-in-remedy()
  (interactive)
  (async-shell-command-no-window (format "remedybg.exe open-file \"%s\" %i" (buffer-file-name) (line-number-at-pos))))

(defun remedy-start-debugging()
  (interactive)
  (async-shell-command-no-window "remedybg.exe start-debugging" ))

(defun remedy-stop-debugging()
  (interactive)
  (async-shell-command-no-window "remedybg.exe stop-debugging" ))

(defun remedy-continue-debugging()
  (interactive)
  (async-shell-command-no-window "remedybg.exe continue-execution" ))

(defun remedy-add-breakpoint()
  (interactive)
  (async-shell-command-no-window (format "remedybg.exe add-breakpoint-at-file \"%s\" %i " (buffer-file-name) (line-number-at-pos)) ))

(defun remedy-run-to-point()
  (interactive)
  (async-shell-command-no-window (format "remedybg.exe run-to-cursor \"%s\" %i " (buffer-file-name) (line-number-at-pos)) ))



(add-to-list 'compilation-error-regexp-alist 'casey-devenv)

(add-to-list 'compilation-error-regexp-alist-alist '(casey-devenv
						     "*\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)): \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
						     2 3 nil (4)))


(setq-default compilation-error-regexp-alist
	      (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
		    compilation-error-regexp-alist))

(bind-key* "C-x n" 'compilation-next-error 'compilation-mode-map)
(bind-key* "C-x n" 'next-error)


(bind-key* "C-x d" 'open-file-in-remedy)

(bind-key* "<f9>" 'remedy-add-breakpoint)
(bind-key* "C-c p l" 'remedy-start-debugging)
(bind-key* "C-c p s" 'remedy-stop-debugging)
(bind-key* "S-<f5>" 'remedy-stop-debugging)

(bind-key* "C-c p C" 'recompile)

(bind-key* "C-c l" 'ov-clear)

(defun my-compile (action)
  (setq compile-command action)
  (setq compilation-read-command 'nil)
  (call-interactively 'compile))

(defun my-edit-compile (action)
  (setq compile-command action)
  (setq compilation-read-command 't)
  (call-interactively 'compile))

(defun helm-compile ()
  (interactive)
  (setq current-dir (file-name-directory buffer-file-name))
  (cd (projectile-project-root))
  (helm :sources (helm-build-in-file-source "Compile Commands"
		     (concat (file-name-as-directory (projectile-project-root)) ".compile")
		   :action '(("Compile" . my-compile)
			     ("Edit and Compile" . my-edit-compile)))
	:buffer "*helm compile*")
  (cd current-dir))

(defun close-compile-buffer ()
  (interactive)
  (delete-window (get-buffer-window "*compilation*")))

(defun find-file-at-point-goto-line (ret)
  "Ignore RET and jump to line number given in `ffap-string-at-point'."
  (when (and
     (stringp ffap-string-at-point)
     (string-match ":\\([0-9]+\\)\\'" ffap-string-at-point))
    (goto-char (point-min))
    (forward-line (string-to-number (match-string 1 ffap-string-at-point))))
  ret)

(advice-add 'find-file-at-point :filter-return #'find-file-at-point-goto-line)

(defun my-go-to (action)
  (let* ((parts (helm-grep-split-line action))
	 (file (nth 0 parts))
	 (line (string-to-number (nth 1 parts))))
    (find-file file)
    (goto-line line)))

(require 'subr-x)

(defun helm-type ()
  (interactive)
  (setq current-dir (file-name-directory buffer-file-name))
  (cd (projectile-project-root))
  (helm :sources (helm-build-in-file-source "Types in project"
		     (concat (file-name-as-directory (projectile-project-root)) ".types")
		   :action '(("Go to" . my-go-to))
		   :real-to-display (lambda (line) (string-trim-left (nth 2 (helm-grep-split-line line))))
		   :get-line #'buffer-substring
		   )
	:buffer "*helm types*")
  (cd current-dir))


(bind-key* "C-c p c" 'helm-compile)
(bind-key* "C-c p t" 'helm-type)

(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-study-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-note-face)
(make-face 'font-lock-done-face)
(mapc (lambda (mode)
	(font-lock-add-keywords
	 mode
	 '(
	   ("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
	   ("\\<\\(Todo\\)" 1 'font-lock-fixme-face t)
	   ("\\<\\(STUDY\\)" 1 'font-lock-study-face t)
	   ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
	   ("\\<\\(Done\\)" 1 'font-lock-done-face t)
	   ("\\<\\(DONE\\)" 1 'font-lock-done-face t)
	   ("\\<\\(NOTE\\)" 1 'font-lock-note-face t)
	   ("\\<\\(Note\\)" 1 'font-lock-note-face t))))
      fixme-modes)

(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-done-face "Chartreuse"  nil nil t nil t nil nil)
(modify-face 'font-lock-study-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-important-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)


(require 'custom)
(require 'ov)

(defvar all-overlays ())

(defun delete-this-overlay(overlay is-after begin end &optional len)
  (delete-overlay overlay))

(defun highlight-current-line (s err)
  (interactive)
  (setq current-point (point))
  (beginning-of-line)
  (setq beg (point))
  (end-of-line)
  (setq end (point))

  (forward-line 1)
  (setq true-end (point))

  (setq error-line-overlay (make-overlay 1 1))
  (setq whole-line-overlay (make-overlay 1 1))

  (setq all-overlays (cons whole-line-overlay all-overlays))
  (setq all-overlays (cons error-line-overlay all-overlays))

  (setq face-color (if err "#8b0000" "#8b4500"))
  (setq msg s)

  (overlay-put error-line-overlay
	       'face `(:background ,face-color))

  (overlay-put whole-line-overlay
	       'face `(:background ,face-color))

  (overlay-put error-line-overlay
	       'after-string
	       (propertize (format "\t\t %s" msg)
			   'face `(:background ,face-color :height 80 :weight semi-bold)))

  (overlay-put error-line-overlay
	       'modification-hooks (list 'delete-this-overlay))

  (overlay-put whole-line-overlay
	       'modification-hooks (list 'delete-this-overlay))

  (move-overlay error-line-overlay beg end)
  (move-overlay whole-line-overlay beg true-end)
  (goto-char current-point))

(defun delete-all-overlays()
  (while all-overlays
    (delete-overlay (car all-overlays))
    (setq all-overlays (cdr all-overlays))))

(defun highlight-error-lines(compile-buffer msg)
  (interactive)
  (delete-all-overlays)
  (condition-case nil
      (while t
	(save-excursion
	(next-error)
	(compilation-next-error-function 0)
	(with-current-buffer compile-buffer
	  (end-of-line)
	  (setq max-reg (point))
	  (beginning-of-line)
	  (if (search-forward ": error " max-reg t)
	      (setq err t)
	    (setq err nil)
	    (search-forward ": warning " max-reg t))
	  (search-forward ":")
	  (setq beg (point))
	  (forward-line)
	  (setq end (- (point) 1))
	  (setq compile-message (buffer-substring beg end)))
	(highlight-current-line compile-message err)))
    (error nil))
  (condition-case nil
      (first-error)
    (error nil)))

(add-to-list 'compilation-finish-functions 'highlight-error-lines)
(add-hook 'compilation-start-hook '(lambda (proc) (ov-clear)))
;; (setq compilation-finish-function 'highlight-error-lines)

(add-hook 'write-file-hooks 'delete-trailing-whitespace nil t)


;; (setq compilation-finish-functions nil)
;; (setq compilation-start-hook nil)


(setq pixel-scroll-precision-mode 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 5) ;; keyboard scroll one line at a time


(defun ag-in-current-dir (string)
  (interactive (list (ag/read-from-minibuffer "Search string")))
  (ag/search string default-directory))

(defun my-random-sort-lines (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))
