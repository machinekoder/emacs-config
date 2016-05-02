;; This is the PhyxCalc Mode for Emacs
(defun phyxcalc-start ()
  "Starts the phyxcalc process"

  (interactive)
  (when (not (eq (get-process "phyxcalc") nil))
    (delete-process "phyxcalc")
    )
  (let ((process-connection-type nil))  ; use a pipe
    (start-process "phyxcalc" "*phyxcalc*" "~/projects/build-PhyxCalc-Desktop_Qt_5_4_GCC_64bit-Release/phyxcalc"))
)

(defun phyxcalc-stop ()
  (interactive)
  (when (not (eq (get-process "phyxcalc") nil))
    (delete-process "phyxcalc")
    )
  (when (not (eq (get-buffer "*phyxcalc*") nil))
    (let ((buffer-modified-p nil))
      (kill-buffer "*phyxcalc*"))
    )
  )

(defun s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun s-trim (s)
  "Remove whitespace at the beginning and end of S."
    (s-trim-left (s-trim-right s)))

(defun phyxcalc-input ()
  "Input current line into phyxcalc"

  (interactive)
  (let (myLine)
  ;; grab the current line
    (setq myLine (thing-at-point 'line))
    (process-send-string "phyxcalc" myLine)
    (accept-process-output "phyxcalc" 1 0 t)
    )
  )

(defun phyxcalc-get-current-line ()
  "gets the context of the next line"

  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point)))))
  )
    ;;(setq currentLine (phyxcalc-get-current-line))
    ;;(goto-char (point-min)) (forward-line (+ currentLine 0))

(defun phyxcalc-eval (expression)
  "Evaluate an expression using phyxcalc"

  (let* ((proc (get-buffer-process "*phyxcalc*"))
         (old-proc-filter (process-filter proc))
         (last-len 0))
    (setq *ret* nil)
    (set-process-filter
     proc
     (lambda (proc str)
       (setq *ret* (concat *ret* str))))
    (process-send-string "phyxcalc" expression)
    (process-send-string "phyxcalc" "\n")
    (accept-process-output proc 0 100)
    (while (not (= (length *ret*) last-len))
      (setq last-len (length *ret*))
      (accept-process-output proc 0 100) )
      ;;(set-process-filter proc old-proc-filter)
      ;;(setq *ret* (cadddr (split-string *ret* "\n")))
      ;;(when (string-match "[ \t]*$" *ret*)
    ;;  (setq *ret* (replace-match "" nil nil *ret*)))
      (s-trim *ret*)
      )
  )

(defun phyxcalc-get-current-line ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position))
  )

(defun phyxcalc-newline ()
  (interactive)
  (end-of-line)
  (newline)
  )

(defun phyxcalc-delete ()
  (interactive)
  (let (currentLine)
    (setq currentLine (phyxcalc-get-current-line))
    (when (string-match "=.*" currentLine)
          (beginning-of-line)
          (kill-line)
          )
      (delete-backward-char 1)
    )
  )

(setq phyxcalc-err-overlay nil)

(defun phyxcalc-error-mark (start end)
  (when (not (eq phyxcalc-err-overlay nil))
    (delete-overlay phyxcalc-err-overlay))
  (setq phyxcalc-err-overlay (make-overlay start end))
  (overlay-put phyxcalc-err-overlay 'face '(:background "red"))
  )

(defun phyxcalc-error-delete ()
  (when (not (eq phyxcalc-err-overlay nil))
    (delete-overlay phyxcalc-err-overlay)
    (setq phyxcalc-err-overlay nil))
)

(defun phyxcalc-eval-line ()
  (interactive)
  (let (currentLine nextLine result) ;; define local variables
    (setq currentLine (phyxcalc-get-current-line)) ;; get current line
    (if (not (string-match "^=.*" currentLine)) ;; check for equal sign
      (progn
        (phyxcalc-error-delete) ;; delete error when existing
        (setq result (phyxcalc-eval currentLine))
        (when (string-match "^=.*" result)
          (when (= (forward-line) 1)
              (newline)
            )
          (setq nextLine (phyxcalc-get-current-line))
          (if (string-match "^=.*" nextLine)
              (kill-line)
            (progn
              (newline)
              (forward-line -1)
              )
            )
          (insert result)
          (forward-line)
          )
        (when (string-match "error: position \\([0-9]+\\)-\\([0-9]+\\), \\(.*\\)" result)
          (progn
            (let (start end)
              (setq start (+
                           (line-beginning-position)
                           (string-to-number (match-string 1 result))))
              (setq end (+
                         (line-beginning-position)
                         (string-to-number (match-string 2 result))))
              (phyxcalc-error-mark start end)
              (message (match-string 3 result))
              )
            )
          )
        )
      ;; no equal sign
      (progn
        (end-of-line)
        (newline)
        )
      )
    )
  )

(setq phyxcalc-mode-map (make-sparse-keymap))

(define-derived-mode phyxcalc-mode fundamental-mode
  "phyxcalc-mode"
  "Major moe for editing phyxcalc files"
  )

(add-hook
 'phyxcalc-mode-hook
 (lambda ()
   (define-key phyxcalc-mode-map (kbd "RET") (lambda ()
                                               (interactive)
                                               (phyxcalc-eval-line)
                                               )
     )
   (define-key phyxcalc-mode-map (kbd "M-RET") (phyxcalc-newline))
   (define-key phyxcalc-mode-map (kbd "DEL") (phyxcalc-delete))
   )
 )


