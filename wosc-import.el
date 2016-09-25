;;; wosc-import.el --- Organize Python imports

;; Copyright 2012-2016 Wolfgang Schnerring

;; Author: Wolfgang Schnerring <wosc@wosc.de>
;; URL: https://bitbucket.org/wosc/import-el
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; License:

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; Quick start:
;;    (autoload 'wosc-sort-imports "wosc-import" "Sort imports")
;;    (autoload 'wosc-create-import "wosc-import" "Create import")
;;
;;    (add-hook 'python-mode-hook
;;              (lambda ()
;;                (define-key python-mode-map [f6] 'wosc-sort-imports)
;;                (define-key python-mode-map [f7] 'wosc-create-import)))
;;
;; See the README for more details.

;;; Code:

(defconst wosc-import-line "^\\(import \\)\\|\\(from.*import\\)")
(defconst wosc-import-or-blank (concat wosc-import-line "\\|$"))

;;;###autoload
(defun wosc-sort-imports ()
  "Sorts import lines alphabetically, from the beginning of the
buffer to the first non-blank, non-import line."
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
  (while (and (looking-at wosc-import-or-blank)
             (equal (forward-line 1) 0)) nil)
  (move-beginning-of-line nil)
  (if (not (looking-at wosc-import-line))
      ; we're on the first non-blank, non-import line
      (forward-line -1))
  (while (and (looking-at "^$")
              (equal (forward-line -1) 0)) nil)
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

(defun _wosc-create-import (package)
  (if (wosc-import-exists package)
      (progn
        (message "import %s already exists" package)
        (deactivate-mark))
    (progn
      (save-excursion
        (goto-char (point-min))
        (re-search-forward wosc-import-line nil t)
        (move-beginning-of-line nil)
        (open-line 1)
        (insert "import " package)
        (wosc-sort-imports)))
    )
  )

;;;###autoload
(defun wosc-create-import (start end)
  "Prompts for a package name and creates a import line for it,
then calls wosc-sort-imports. In transient mark mode, the current
region is used as the package name instead of prompting."
  (interactive "r")
  (let ((package (if (and transient-mark-mode mark-active)
                     (buffer-substring start end)
                   (wosc-guess-package-at-point))))
    (_wosc-create-import package))
  )

;;; wosc-import.el ends here
