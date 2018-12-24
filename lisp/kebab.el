;; camelcase

(defun un-camelcase-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
   Default for SEP is a hyphen \"-\".
   If third argument START is non-nil, convert words after that
   index in STRING."
  (let ((done-first nil)
        (case-fold-search nil))
    (while (string-match "[A-ZΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ]" s (or start 0))
      (if done-first
        (setq s (replace-match (concat (or sep "-")
                                       (downcase (match-string 0 s)))
                               t nil s))
        (progn
          (setq s (replace-match (downcase (match-string 0 s)) t nil s))
          (setq done-first 't))))
    (downcase (s-replace "--" "-" s))))

(defun un-camelcase-region ()
  (interactive)
  (let ((s (buffer-substring (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert (un-camelcase-string s))))

(defun un-camelcase-symbol ()
  (interactive)
  (save-excursion
    (let ((s (format "%s" (symbol-at-point)))
          (bounds (bounds-of-thing-at-point 'symbol)))
      (let ((replacement (un-camelcase-string s)))
        (when replacement
          (delete-region (car bounds) (cdr bounds))
          (insert replacement))))))

(defun camel->kebab ()
  (interactive)
  (un-camelcase-region))
(provide 'kebab)
