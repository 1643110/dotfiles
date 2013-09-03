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
(require 'cl)

;====================================
;;jaspace.el を使った全角空白、タブ、改行表示モード
;;切り替えは M-x jaspace-mode-on or -off
;====================================
;(require 'jaspace)
;; ;; 全角空白を表示させる。
;; (global-whitespace-mode 1)
;; ;; スペースの定義は全角スペースとする。
;; (setq whitespace-space-regexp "\x3000+")

;; ;; 改行の色を変更
;; (set-face-foreground 'whitespace-newline "gray40")

;; ;; 半角スペースと改行を除外
;; (dolist (d '((space-mark ?\ ) (newline-mark ?\n)))
;;   (setq whitespace-display-mappings
;;         (delete-if
;;          '(lambda (e) (and (eq (car d) (car e))
;;                            (eq (cadr d) (cadr e))))
;;          whitespace-display-mappings)))

;; ;; 全角スペースと改行を追加
;; (dolist (e '((space-mark   ?\x3000 [?\□])
;;              (newline-mark ?\n     [?\u21B5 ?\n] [?$ ?\n])))
;;   (add-to-list 'whitespace-display-mappings e))

;; ;; 強調したくない要素を削除
;; (dolist (d '(face lines space-before-tab
;;                   indentation empty space-after-tab tab-mark))
;;   (setq whitespace-style (delq d whitespace-style)))

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
