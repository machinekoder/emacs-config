(require 'generic-x)

(define-generic-mode gcode-generic-mode
  '((";") ("("))
  (apply 'append
         (mapcar #'(lambda (s) (list (upcase s) (downcase s) (capitalize s)))
                 '("sub" "endsub" "if" "do" "while" "endwhile" "call" "endif"
                   "sqrt" "return" "mod" "eq" "ne" "gt" "ge" "lt" "le" "and"
                   "or" "xor" "atan" "abs" "acos" "asin" "cos" "exp"
                   "fix" "fup" "round" "ln" "sin" "tan" "repeat" "endrepeat")))
  '(("\\(#<_?[A-Za-z0-9_]+>\\)" (1 font-lock-type-face))
    ("\\([NnGgMmFfSsTtOo]\\)" (1 font-lock-function-name-face))
    ("\\([XxYyZzAaBbCcUuVvWwIiJjKkPpQqRr]\\)" (1 font-lock-string-face))
    ("\\([\-+]?[0-9]*\\.[0-9]+\\)" (1 font-lock-constant-face))
    ("\\(#[0-9]+\\)" (1 font-lock-type-face))
    ("\\([0-9]+\\)" (1 font-lock-constant-face)))
  '("\\.ngc\\'")
  nil
  "Generic mode for g-code files.")

;; disable electric indent
(add-hook 'gcode-generic-mode-hook (lambda () (electric-indent-local-mode -1)))

(provide 'gcode-mode)
