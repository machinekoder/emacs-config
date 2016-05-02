;; gsl-mode
(defface hi-gsl-com
  '((((background dark)) (:background "gray21" :foreground "gray48"))
    (t (:weight medium :background "gray21")))
  "Face for gsl-minor-mode."
  :group 'hi-lock-faces)

(defface hi-gsl-code
  '((((background dark)) (:background "gray21" :foreground "green"))
    (t (:weight medium :background "gray21")))
  "Face for gsl-minor-mode."
  :group 'hi-lock-faces)

(define-minor-mode gsl-minor-mode
  "Highlight imatix/GSL code."
  :global t
  :lighter " gsl"
  (if gsl-minor-mode
      (highlight-regexp "\\(^\\..*\\)\n" 'hi-gsl-code)
    (unhighlight-regexp "\\(^\\..*\\)\n"))
  (if gsl-minor-mode
      (highlight-regexp "\\(\\$(.*?)\\)" 'hi-red-b)
    (unhighlight-regexp "\\(\\$(.*?)\\)"))
  (if gsl-minor-mode
      (highlight-regexp "\\(^\\.-.*\\)\n" 'hi-gsl-com)
    (unhighlight-regexp "\\(^\\.-.*\\)\n")))

(provide 'gsl-mode)
