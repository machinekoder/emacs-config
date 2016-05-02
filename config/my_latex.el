(add-hook 'latex-mode-hook 'outline-minor-mode)
(global-set-key (kbd "C-<tab>") 'outline-toggle-children)

(require 'tex-buf)
(defun TeX-command-default (name)
  "Next TeX command to use. Most of the code is stolen from `TeX-command-query'."
  (cond ((if (string-equal name TeX-region)
             (TeX-check-files (concat name "." (TeX-output-extension))
                              (list name)
                              TeX-file-extensions)
           (TeX-save-document (TeX-master-file)))
         TeX-command-default)
        ((and (memq major-mode '(doctex-mode latex-mode))
              (TeX-check-files (concat name ".bbl")
                               (mapcar 'car
                                       (LaTeX-bibliography-list))
                               BibTeX-file-extensions))
         ;; We should check for bst files here as well.
         TeX-command-BibTeX)
        ((TeX-process-get-variable name
                                   'TeX-command-next
                                   TeX-command-Show))
        (TeX-command-Show)))


(defcustom TeX-texify-Show t "Start view-command at end of TeX-texify?" :type 'boolean :group 'TeX-command)
(defcustom TeX-texify-max-runs-same-command 5 "Maximal run number of the same command" :type 'integer :group 'TeX-command)

(defun TeX-texify-sentinel (&optional proc sentinel)
  "Non-interactive! Call the standard-sentinel of the current LaTeX-process.
If there is still something left do do start the next latex-command."
  (set-buffer (process-buffer proc))
  (funcall TeX-texify-sentinel proc sentinel)
  (let ((case-fold-search nil))
    (when (string-match "\\(finished\\|exited\\)" sentinel)
      (set-buffer TeX-command-buffer)
      (unless (plist-get TeX-error-report-switches (intern (TeX-master-file)))
        (TeX-texify)))))

(defun TeX-texify ()
  "Get everything done."
  (interactive)
  (let ((nextCmd (TeX-command-default (TeX-master-file)))
        proc)
    (if (and (null TeX-texify-Show)
             (equal nextCmd TeX-command-Show))
        (when  (called-interactively-p 'any)
          (message "TeX-texify: Nothing to be done."))
      (TeX-command nextCmd 'TeX-master-file)
      (when (or (called-interactively-p 'any)
                (null (boundp 'TeX-texify-count-same-command))
                (null (boundp 'TeX-texify-last-command))
                (null (equal nextCmd TeX-texify-last-command)))
        (mapc 'make-local-variable '(TeX-texify-sentinel TeX-texify-count-same-command TeX-texify-last-command))
        (setq TeX-texify-count-same-command 1))
      (if (>= TeX-texify-count-same-command TeX-texify-max-runs-same-command)
          (message "TeX-texify: Did %S already %d times. Don't want to do it anymore." TeX-texify-last-command TeX-texify-count-same-command)
        (setq TeX-texify-count-same-command (1+ TeX-texify-count-same-command))
        (setq TeX-texify-last-command nextCmd)
        (and (null (equal nextCmd TeX-command-Show))
             (setq proc (get-buffer-process (current-buffer)))
             (setq TeX-texify-sentinel (process-sentinel proc))
             (set-process-sentinel proc 'TeX-texify-sentinel))))))

(add-hook 'LaTeX-mode-hook '(lambda () (local-set-key (kbd "C-c C-a") 'TeX-texify)))



;; Okular
(setq TeX-PDF-mode t) ;; use pdflatex instead of latex

(setq-default TeX-master nil)
;; ##### Enable synctex correlation. From Okular just press
;; ##### Shift + Left click to go to the good line.
(setq TeX-source-correlate-method 'synctex)
;; ##### Enable synctex generation. Even though the command shows
;; ##### as "latex" pdflatex is actually called
(custom-set-variables '(LaTeX-command "latex -synctex=1") )

;; ##### Use Okular to open your document at the good
;; ##### point. It can detect the master file.
(add-hook 'LaTeX-mode-hook '(lambda ()
                  (add-to-list 'TeX-expand-list
                       '("%u" Okular-make-url))))

(defun Okular-make-url () (concat
               "file://"
               (expand-file-name (funcall file (TeX-output-extension) t)
                         (file-name-directory (TeX-master-file)))
               "#src:"
               (TeX-current-line)
               (expand-file-name (TeX-master-directory))
               "./"
               (TeX-current-file-name-master-relative)))

;; ## Use these lines if you want a confirmation of the
;; ## command line to run...
;; (setq TeX-view-program-selection '((output-pdf "Okular")))
;; (setq TeX-view-program-list '(("Okular" "okular --unique %u")))
;; ## And theses if you don't want any confirmation.
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
        '("View" "okular --unique %u" TeX-run-discard-or-function nil t :help "View file")))

;;master
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (setq-default TeX-master "master") ; All master files called "master".
  (setq-default TeX-PDF-mode t)
  (setq-default TeX-source-correlate-mode t)
  ;;(when (require 'latex nil t)
(push '("%(masterdir)" (lambda nil (file-truename (TeX-master-directory))))
      TeX-expand-list)
(push '("Okular" "okular --unique %o#src:%n%(masterdir)./%b")
      TeX-view-program-list)
(push '(output-pdf "Okular") TeX-view-program-selection)
;;)

(provide 'my_latex)
