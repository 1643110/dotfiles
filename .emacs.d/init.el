;; set language Japanese
(set-language-environment 'Japanese)
;; UTF-8
(prefer-coding-system 'utf-8)

;; set window status
(if window-system (progn
(setq initial-frame-alist '((width . 100)(height . 45)(top . 0)(left . 0)))
(set-background-color "Black")
(set-foreground-color "White")
(set-cursor-color "Gray")
))

;; make window transparent(windowの透明化)
(set-frame-parameter nil 'alpha 85)


;====================================
; inintialzation
;====================================

;; install redo+.el 
;(install-elisp "http://www.emacswiki.org/emacs/download/redo+.el")

;; タブ, 全角スペース, 行末空白表示
(defface my-face-b-1 '((t (:background "NavajoWhite4"))) nil) ; 全角スペース
(defface my-face-b-2 '((t (:background "gray10"))) nil)	      ; タブ
(defface my-face-u-1 '((t (:background "SteelBlue" :underline t))) nil) ; 行末空白
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
 (font-lock-add-keywords
 major-mode
 '(("\t" 0 my-face-b-2 append)
 ("　" 0 my-face-b-1 append)
 ("[ \t]+$" 0 my-face-u-1 append)
 )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)


;(set-frame-parameter nil 'alpha 85)

;; 行番号表示
(require 'linum)
(global-linum-mode)

;; cua-modeの設定
(cua-mode t) ; cua-modeをオン
(setq cua-enable-cua-keys nil) ; CUAキーバインドを無効化
(define-key cua-global-keymap (kbd "<C-return>") nil)
(global-set-key (kbd "C-c RET") 'cua-set-rectangle-mark)


;; 折り返し表示（標準は折り返さないが、C-c C-lで切り替え可能）
(setq-default truncate-partial-width-windows t)
(setq-default truncate-lines t)
(global-set-key "\C-c\C-l" 'toggle-truncate-lines)


;; マウスで選択するとコピーする Emacs 24 ではデフォルトが nil
(setq mouse-drag-copy-region t)

;;(menu-bar-mode nil)  ;; メニューバー非表示
(tool-bar-mode -1)   ;; ツールバー非表示

;; Emacs Lisp パッケージ追加
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(setq load-path (cons "~/.emacs.d/elisp" load-path))

;;auto-installの設定
(when(require 'auto-install nil t)
  ;;インストールディレクトリを設定する　初期値は~/.emacs.d/auto-install/
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;;EmacsWikiに登録されているelispの名前を取得する
  (auto-install-update-emacswiki-package-name t)
  ;;必要であればプロキシの設定を行う
  ;;(setq url-proxy-services '(("http" . "localhost:8339")))
  ;;install-elispの関数を利用可能にする
  (auto-install-compatibility-setup)
)

;; redo+の設定(何故が使えなかったので、undo-treeに乗り換えてみる)
;; (when (require 'redo+ nil t)
;;   (global-set-key (kbd "C-'") 'redo)
;;   ;;(global-set-key (kbd "C-.") 'redo) ;;JIS Keyboard
;; )
;; undo-treeの設定(M-x list-packagesでundo-treeをインストール)
(when (require 'undo-tree nil t)
  (global-undo-tree-mode t)
  (global-set-key (kbd "M-/") 'undo-tree-redo)
)

;; 自動補完機能の設定
(require 'auto-complete)
(require 'auto-complete-config)
;; グローバルでauto-completeを利用
(global-auto-complete-mode t)
(define-key ac-completing-map (kbd "M-n") 'ac-next)      ; M-nで次候補選択
(define-key ac-completing-map (kbd "M-p") 'ac-previous)  ; M-pで前候補選択
(setq ac-dwim t)  ; 空気読んでほしい
;; 情報源として
;; * ac-source-filename
;; * ac-source-words-in-same-mode-buffers
;; を利用
(setq-default ac-sources '(ac-source-filename ac-source-words-in-same-mode-buffers))
;; また、Emacs Lispモードではac-source-symbolsを追加で利用
(add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols t)))
;; 以下、自動で補完する人用
(setq ac-auto-start 3)

;;================================
;;
;;     Ruby on Rails 
;;
;;================================
;; rinariの設定
(add-to-list 'load-path "~/.emacs.d/elisp/rinari")
(require 'rinari nil t)

;;rhtml-modeの設定
(add-to-list 'load-path "~/.emacs.d/elisp/rhtml")
(when (require 'rhtml-mode nil t)
  (add-hook 'rhtml-mode-hook
	    (lambda () (rinari-launch)))
)

;; rails-yasnippetのロード
;(require 'yasnippet)
;(yas/initialize)
;(yas/load-directory "~/.emacs.d/elisp/yasnippets-rails/rails-snippets")


;;================================
;;
;;     anything
;;
;;================================
;; anything の設定
(when (require 'anything-startup nil t)
  (global-set-key (kbd "\C-x b") 'anything)
)

;; killringの履歴を表示する
(global-set-key (kbd "M-y") 'anything-show-kill-ring)
