(load "../wosc-import.el")

(defun this-line ()
  (save-excursion
    (let ((start (progn (move-beginning-of-line nil) (point)))
          (end (progn (move-end-of-line nil) (point))))
      (buffer-substring start end))
    )
  )

(ert-deftest creates-import-line ()
             (with-temp-buffer
               (insert "import zoo\\nfoo\n")
               (_wosc-create-import "foo.bar")
               (goto-char (point-min))
               (should (equal "import foo.bar" (this-line)))
               )
)
