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

(let ((default-directory (expand-file-name "~/.emacs.d/elisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

(load-theme 'misterioso t)
;;(load-theme 'light-blue t)

;;行ハイライト
(global-hl-line-mode)
(set-face-background 'hl-line "#1C1C")
(setq hl-line-face 'underline)

;; 起動時、Welcome to GNU Emacs画面非表示
(setq inhibit-startup-message t)

;; (require 'ido)
;; (ido-mode t)

;;------------------------------------- init loader化したい ---------------------------------------------------------
;; Mode line setup
(setq-default
 mode-line-format
 '("%e"
   mode-line-mule-info
   " "
   ; Position, including warning for 80 columns
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   ; emacsclient [default -- keep?]
   mode-line-client
   " "
   ; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t " ")))
   " "
   ; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ; narrow [default -- keep?]
   " %n "
   ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   " %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   " "
   ; nyan-mode uses nyan cat as an alternative to %p
   (:eval (when nyan-mode (list (nyan-create))))
   ))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line nil
    :foreground "gray60" :background "blue"
    :inverse-video nil
    :box '(:line-width 6 :color "gray20" :style nil))
(set-face-attribute 'mode-line-inactive nil
    :foreground "gray80" :background "gray40"
    :inverse-video nil
    :box '(:line-width 6 :color "gray40" :style nil))

(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829"
    :background "#ffffff"
    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "gray60")
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#eab700"
    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :family "Menlo" :height 100)
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray40"
    :height 110)
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")
;;------------------------------------- init loader化したい ---------------------------------------------------------

;; バッファ切り替え
(global-set-key "\C-t" 'other-window)

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
;;(tool-bar-mode -1)   ;; ツールバー非表示


;; windows resize
(defun my-window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        c)
    (catch 'end-flag
      (while t
        (message "size[%dx%d]"
                 (window-width) (window-height))
        (setq c (read-char))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (message "Quit")
               (throw 'end-flag t))))))
)
(global-set-key "\C-c\C-r" 'my-window-resizer)

;====================================
;; File-Info - ファイル情報
;====================================
(defun file-info () "
カレントバッファのファイル情報を表示する。"
  (interactive)
  (if (buffer-file-name (current-buffer))
      (progn
        (let* ((file-name (buffer-file-name (current-buffer)))
               (f-attr (file-attributes file-name))
               (a-time (nth 4 f-attr))  ; 最終アクセス時刻
               (m-time (nth 5 f-attr))  ; 最終修正時刻
               (f-size (nth 7 f-attr))  ; ファイルサイズ
               (f-mode (nth 8 f-attr))  ; ファイル属性
               (mes1 (format "ファイルパス:   %s\n" file-name))
               (mes2 (format "最終参照時刻:   %s\n"
                              (format-time-string
                               "%Y-%m-%d %H:%M:%S" a-time)))
               (mes3 (format "最終修正時刻:   %s\n"
                              (format-time-string
                               "%Y-%m-%d %H:%M:%S" m-time)))
               (mes4 (format "ファイルサイズ: %s byte\n" f-size))
               (mes5 (format "ファイル属性:   %s" f-mode))
               (mess (concat mes1 mes2 mes3 mes4 mes5)))
          (message "%s" mess)))
    nil ))


;====================================
; Emacs Lisp パッケージ追加
;====================================
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(setq load-path (cons "~/.emacs.d/elisp" load-path))

(require 'cl)
(defvar installing-package-list
  '(
    ;; ここに使っているパッケージを書く。
    scala-mode2
    ruby-mode
    markdown-mode
    scss-mode
    yaml-mode
    anything
    auto-install
    undo-tree
    auto-complete
    tabbar
    direx
    google-translate
    magit
    popwin
    ))

(let ((not-installed (loop for x in installing-package-list
                            when (not (package-installed-p x))
                            collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
        (package-install pkg))))

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

;; undo-treeの設定
(when (require 'undo-tree nil t)
  (global-undo-tree-mode t)
  (global-set-key (kbd "M-/") 'undo-tree-redo))

;; 自動補完機能の設定
(when (require 'auto-complete nil t)
  (require 'auto-complete-config)
  ;; グローバルでauto-completeを利用
  (global-auto-complete-mode t)
  (define-key ac-completing-map (kbd "M-n") 'ac-next)
  (define-key ac-completing-map (kbd "M-p") 'ac-previous)
  (setq ac-dwim t)  ; 空気読んでほしい
  ;; 情報源として
  ;; * ac-source-filename
  ;; * ac-source-words-in-same-mode-buffers
  ;; を利用
  (setq-default ac-sources '(ac-source-filename ac-source-words-in-same-mode-buffers))
  ;; また、Emacs Lispモードではac-source-symbolsを追加で利用
  (add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols t)))
  ;; 以下、自動で補完する人用
  (setq ac-auto-start 3))

;;================================
;;          Ruby
;;================================
(when (require 'ruby-mode nil t)
  (defun ruby-mode-set-encoding () ()))	;magic commentの無効化

;;================================
;;     Ruby on Rails 
;;================================
;; rinariの設定
(add-to-list 'load-path "~/.emacs.d/elisp/rinari")
(require 'rinari nil t)

;;rhtml-modeの設定
(add-to-list 'load-path "~/.emacs.d/elisp/rhtml")
(when (require 'rhtml-mode nil t)
  (add-hook 'rhtml-mode-hook
	    (lambda () (rinari-launch))))

;; rails-yasnippetのロード
;(require 'yasnippet)
;(yas/initialize)
;(yas/load-directory "~/.emacs.d/elisp/yasnippets-rails/rails-snippets")


;;================================
;;     scala
;;================================
;; scala-mode2
(when (require 'scala-mode2 nil t)
  ;; ENSIME for scala
  (add-to-list 'load-path "~/.emacs.d/elisp/ensime/elisp/")
  (when (require 'ensime nil t)
    (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)))

;;================================
;;     tabbar
;;================================
(when (require 'tabbar nil t)
  (tabbar-mode 1)
  ;; グループ化しない
  (setq tabbar-buffer-groups-function nil)
  ;; 左に表示されるボタンを無効化
  (dolist (btn '(tabbar-buffer-home-button
		 tabbar-scroll-left-button
		 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil)
		   (cons "" nil))))
  ;; タブ同士の間隔
  (setq tabbar-separator '(1.2))
  ;; 外観変更
  (set-face-attribute			;バー自体の色
   'tabbar-default nil
   :family (face-attribute 'default :family)
   :background "white"
   :height 0.9)
  (set-face-attribute  			;アクティブなタブ
   'tabbar-selected nil
   :background "black"
   :foreground "white"
   :weight 'bold
   :box nil)
  (set-face-attribute			;非アクティブなタブ
   'tabbar-unselected nil
   :background "white"
   :foreground "black"
   :box nil))

;;================================
;;     anything
;;================================
;; anything の設定
(when (require 'anything-startup nil t)
  (global-set-key (kbd "\C-x b") 'anything)
;;  (global-set-key (kbd "C-i") 'anything-imenu)
  ;; killringの履歴を表示する
  (global-set-key (kbd "M-y") 'anything-show-kill-ring))

;;================================
;;     Git
;;================================
;; magit の設定
(when (require 'magit nil t))

;;================================
;;     direx
;;================================
(when (require 'direx nil t) (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window) )

;;================================
;;     google-translate
;;================================
(when (require 'google-translate nil t)
  ;; en -> ja
  (defun google-translate-en-ja ()
    (interactive)
    (custom-set-variables
      '(google-translate-default-source-language "en")
      '(google-translate-default-target-language "ja"))
    (google-translate-query-translate))
  ;;ja -> en 
  (defun google-translate-ja-en ()
    (interactive)
    (custom-set-variables
     '(google-translate-default-source-language "ja")
     '(google-translate-default-target-language "en"))
    (google-translate-query-translate))
  ;; keybind
  (global-set-key (kbd "C-c j") 'google-translate-ja-en)
  (global-set-key (kbd "C-c e") 'google-translate-en-ja)
  (global-set-key (kbd "C-c t") 'google-translate-at-point))

;;================================
;;     popwin
;;================================
(when (require 'popwin nil t)
  (setq display-buffer-function 'popwin:display-buffer)
  (push '(direx:direx-mode :position left :width 40 :dedicated t) popwin:special-display-config)
  (push '("*Google Translate*" :position bottom :dedicated t) popwin:special-display-config)
  ;; (push '("*Completions*" :position bottom :dedicated t) popwin:special-display-config)
)
