(setq wosc-import-line "^\\(import \\)\\|\\(from.*import\\)")
(setq wosc-import-or-blank (concat wosc-import-line "\\|$"))

(defun wosc-sort-imports ()
  (interactive)
  (save-excursion
    (let ((start (progn
                   (goto-char (point-min))
                   (re-search-forward wosc-import-line nil t)
                   (move-beginning-of-line nil)))
          (end (progn
                 (wosc-goto-end-of-imports)
                 (point))))
      (if (and start end)
          (progn
            (sort-lines nil start end)
            (flush-lines "^$" start end)
            ))
  )))

(defun wosc-goto-end-of-imports ()
  (while (looking-at wosc-import-or-blank)
    (forward-line 1))
  ; now we're on the first non-blank, non-import line.
  ; go back one and skip backwards over the blanks
  (forward-line -1)
  (while (looking-at "^$")
    (forward-line -1))
  (move-end-of-line nil)
  )

(defun wosc-import-exists (package)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "^import " package "$") nil t)
    )
  )

(defun wosc-guess-package-at-point ()
  (save-excursion
    (let ((end (re-search-backward "\\." nil t))
          (start (re-search-backward "^\\|[ ([]" nil t))
          )
      (buffer-substring start end)
          )
    )
  )

(defun wosc-create-import (start end)
  (interactive "r")
  (let ((package (if (and transient-mark-mode mark-active)
                     (buffer-substring start end)
                   (wosc-guess-package-at-point))))
    (if (wosc-import-exists package)
        (progn
          (message "import %s already exists" package)
          (deactivate-mark))
      (progn
        (save-excursion
          (goto-char (point-min))
          (re-search-forward wosc-import-line nil t)
          (move-beginning-of-line nil)
          (if (looking-at wosc-import-or-blank)
              (wosc-goto-end-of-imports)
            (while (not (looking-at "^$"))
              (forward-line 1)))
          (newline)
          (insert "import " package)
          (wosc-sort-imports)))
      ))
  )
