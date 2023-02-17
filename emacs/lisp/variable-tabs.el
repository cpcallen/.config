;;; variable-tabs.el --- display tab characters with variable width

;; http://emacs.stackexchange.com/questions/12872/is-there-a-way-to-set-a-list-of-display-tab-stops

;;;###autoload
(defun variable-tabs-safe-list-p (x)
  (while (and (consp x)
              (integerp (car x))
              (> (car x) 0))
    (setq x (cdr x)))
  (null x))

;;;###autoload
(defvar variable-tabs-tab-stop-initial-list nil
  "The initial tab stop list for variable tabs.
This is a list of positive integers, which are the successive column widths.
After the columns listed here, `variable-tabs-tab-stop-repeat-list' applies.")
;;;###autoload
(defvar variable-tabs-tab-stop-repeat-list nil
  "The repeated tab stop list for variable tabs.
This is a list of positive integers, which are the successive column widths.
First the columns listed in `variable-tabs-tab-stop-initial-list' apply,
then `variable-tabs-tab-stop-repeat-list' repeats forever.
If this variable is nil, it stands for (`tab-width').")
;;;###autoload
(make-variable-buffer-local 'variable-tabs-tab-stop-initial-list)
;;;###autoload
(make-variable-buffer-local 'variable-tabs-tab-stop-repeat-list)
;;;###autoload
(put 'variable-tabs-tab-stop-initial-list 'safe-local-variable 'variable-tabs-safe-list-p)
;;;###autoload
(put 'variable-tabs-tab-stop-repeat-list 'safe-local-variable 'variable-tabs-safe-list-p)

(defun variable-tabs-next-tab (goal)
  "Return the next variable tab position after GOAL.
First the columns listed in `variable-tabs-tab-stop-initial-list' apply,
then `variable-tabs-tab-stop-repeat-list' repeats forever."
  (let ((i 0)
        (tail variable-tabs-tab-stop-initial-list))
    (while (and tail (<= i goal))
      (setq i (+ i (car tail))
            tail (cdr tail)))
    (cond
     ((> i goal) i)
     (variable-tabs-tab-stop-repeat-list
      (setq tail variable-tabs-tab-stop-repeat-list)
      (while (<= i goal)
        (setq i (+ i (car tail))
              tail (or (cdr tail) variable-tabs-tab-stop-repeat-list)))
      i)
     (t
      (+ i (* tab-width (1+ (/ (- goal i) tab-width))))))))

(defun variable-tabs-compute-alignment (pos)
  (let* ((column (save-excursion
                   (goto-char pos)
                   (current-column)))
         (goal (variable-tabs-next-tab column)))
    `(face nil display (space :align-to ,goal))))

(defun variable-tabs-font-lock ()
  (if (or variable-tabs-tab-stop-initial-list variable-tabs-tab-stop-repeat-list)
      (font-lock-add-keywords nil
                              '(("\t" (0 (variable-tabs-compute-alignment
                                          (match-beginning 0))
                                         t))))))
;;;###autoload
(add-hook 'font-lock-mode-hook 'variable-tabs-font-lock)

;;; The End.
