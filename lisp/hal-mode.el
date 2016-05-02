(require 'generic-x)

(define-generic-mode hal-generic-mode
  '("#")
  (apply 'append
         (mapcar #'(lambda (s) (list (upcase s) (downcase s) (capitalize s)))
                 '("loadrt" "loadusr" "addf" "setp" "sets" "start"
                   "newpin" "newcomp" "sete" "newinst"
                   "newg" "newsig" "ready" "net" "log")))
  '(;;("\\(#<_?[A-Za-z0-9_]+>\\)" (1 font-lock-type-face))
    ("[[:space:]=]+?\\([0-9]+\\(?:\\.?[0-9]+\\)?\\)" (1 font-lock-constant-face))
    ("[[:space:]]+?\\(FALSE\\|TRUE\\)" (1 font-lock-constant-face))
    ("\\(\\[[0-9a-zA-Z_]*\\][0-9a-zA-Z_]+\\)" (1 font-lock-variable-name-face))
    ("[[:space:]]+?\\(u32\\|s32\\|int\\|bit\\|float\\)" (1 font-lock-type-face)))
;;    ("\\([NnGgMmFfSsTtOo]\\)" (1 font-lock-function-name-face))
;;    ("\\([XxYyZzAaBbCcUuVvWwIiJjKkPpQqRr]\\)" (1 font-lock-string-face))
;;
;;    ("\\(#[0-9]+\\)" (1 font-lock-type-face))
;;    ("\\([0-9]+\\)" (1 font-lock-constant-face)))
  '("\\.hal\\'")
  nil
  "Generic mode for HAL files.")

;; disable electric indent
(add-hook 'hal-generic-mode-hook (lambda () (electric-indent-local-mode -1)))

(provide 'hal-mode)
