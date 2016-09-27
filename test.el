(load "wosc-import.el")


(defun this-line ()
  (save-excursion
    (let ((start (progn (move-beginning-of-line nil) (point)))
          (end (progn (move-end-of-line nil) (point))))
      (buffer-substring-no-properties start end))
    )
  )


(ert-deftest wosc-import-test-creates-import-line-in-empty-buffer ()
  (with-temp-buffer
    (wosc--create-import "foo.bar")
    (goto-char (point-min))
    (should (equal "import foo.bar" (this-line)))
    )
  )

(ert-deftest wosc-import-test-creates-import-line-with-no-import-lines ()
  (with-temp-buffer
    (insert "foo\n")
    (wosc--create-import "foo.bar")
    (goto-char (point-min))
    (should (equal "import foo.bar" (this-line)))
    )
  )

(ert-deftest wosc-import-test-creates-import-line-with-import-line ()
  (with-temp-buffer
    (insert "import zoo\nfoo\n")
    (wosc--create-import "foo.bar")
    (goto-char (point-min))
    (should (equal "import foo.bar" (this-line)))
    )
  )

(ert-deftest wosc-import-test-sorts-import-lines ()
  (with-temp-buffer
    (insert "import c\nimport a.b\nimport b\nimport a")
    (wosc-sort-imports)
    (should (equal "import a\nimport a.b\nimport b\nimport c"
                   (buffer-substring (point-min) (point-max))))
    )
  )

(ert-deftest wosc-import-test-sorts-import-lines-with-blanks-after ()
  (with-temp-buffer
    (insert "import c\nimport a.b\nimport b\nimport a\n\n\n\n")
    (wosc-sort-imports)
    (should (equal "import a\nimport a.b\nimport b\nimport c\n\n\n\n"
                   (buffer-substring (point-min) (point-max))))
    )
  )

(ert-deftest wosc-import-test-sorts-import-lines-with-code-after ()
  (with-temp-buffer
    (insert "import c\nimport a.b\nimport b\nimport a\n\nfoo.bar()")
    (wosc-sort-imports)
    (should (equal "import a\nimport a.b\nimport b\nimport c\n\nfoo.bar()"
                   (buffer-substring (point-min) (point-max))))
    )
  )
