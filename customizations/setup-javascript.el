(add-lib-path "js2-mode")
(add-lib-path "js2-refactor")
(add-lib-path "flycheck")
(add-lib-path "xref-js2")

(autoload 'js2-mode "js2-mode" nil t)
(autoload 'js2-jsx-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

(custom-set-variables '(js2-strict-inconsistent-return-warning nil))
(custom-set-variables '(js2-strict-missing-semi-warning nil))

(when (executable-find "eslint_d")
  (setq flycheck-javascript-eslint-executable "eslint_d"))

(add-hook 'js-mode-hook #'setup-js-buffer)

(defun setup-js-buffer ()
  (when (and (derived-mode-p 'js-mode) (not (derived-mode-p 'json-mode)))
    (if buffer-file-name
	(flycheck-mode 1)
      (flycheck-mode -1))
    (js2-minor-mode 1)
    (js2-refactor-mode 1)
    (js2-imenu-extras-mode)
    (indium-interaction-mode 1))

  ;; add eslintd-fix support
  (eslintd-fix-mode)

  ;; add xref-js2 support
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)

  (setq js-indent-level 1)
  (setq js2-indent-level 1)
  (setq js2-basic-offset 1)

  (dolist (mode '(javascript-mode js2-mode js2-jsx-mode))
    (add-to-list 'ac-modes mode)))

