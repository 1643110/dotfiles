;; マウスで選択するとコピーする Emacs 24 ではデフォルトが nil
(setq mouse-drag-copy-region t)

;; Emacs Lisp パッケージ追加
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
