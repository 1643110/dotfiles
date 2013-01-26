;; set language Japanese
(set-language-environment 'Japanese)
;; UTF-8
(prefer-coding-system 'utf-8)

;; マウスで選択するとコピーする Emacs 24 ではデフォルトが nil
(setq mouse-drag-copy-region t)

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
  (auto-install-compatibility-setup))
 
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

;; anything の設定
(when (require 'anything-startup nil t)
  (global-set-key (kbd "\C-x b") 'anything)
)
