;;; Change the language of the citation line in mu4e automatically
;;
;; Description: Change the language of the citation line in mu4e
;; automatically when the ispell dictionary is changed.
;;
;; Author:      Florian Adamsky
;; Maintainer:  Florian Adamsky (concat "fa-emacs" "@" "haktar" ".org")

;; Licence:     Beer-ware (Revision 42)
;;              As long as you retain this notice you can do whatever
;;              you want with this stuff. If we meet some day, and you
;;              think this stuff is worth it, you can buy me a beer in
;;              return
;; Created: 17.04.2014 22:02:44
;; Version: 0.1

(setq languages
     ;; dictionary . locale
      '(("american" . "en_US.utf8")
        ("german8" . "de_AT.utf8")))

(setq de_AT.utf8-message-citation-line-format "Am %A den %d.%m.%Y, %N schrieb:\n")
(setq en_US.utf8-message-citation-line-format "On %A, %b %d %Y, %N wrote:\n")

(defun fa/reverse-nth (list element)
  "This function does the opposite of nth---it returns the number
  of an element in a list."
  (let ((i 0)
        (number-of-element nil))
    (while (and (not number-of-element) (< i (length list)))
      (when (equal element (nth i list))
        (setq number-of-element i))
      (setq i (1+ i)))
    number-of-element))

(defun fa/assoc-next-element (list element)
  "This function searches for an element in a list and gives you
  the next element. If it is the last element, it returns the
  first, like a circular list."
  (let ((n (fa/reverse-nth list element)))
    (if (null n)
        nil
      (if (null (nth (1+ n) list))
          (nth 0 list)
        (nth (1+ n) list)))))

(defun change-message-citation-line (locale)
  (interactive)
  ;; Switch to the buffer *mu4e-headers* and copy the date and the
  ;; sender into variables, since we need it for the citation line.
  (let ((original-date (with-current-buffer "*mu4e-headers*"
                         (mu4e-message-field-at-point :date)))
        (original-from (with-current-buffer "*mu4e-headers*"
                         (concat
                          (caar (mu4e-message-field-at-point :from))
                          " <" (cdar (mu4e-message-field-at-point :from)) ">"))))
    ;; Create the citation line in a temp buffer and save this line to
    ;; a string. This is necessary to find the excat citation line in
    ;; the email.
    (let ((old-citation-line nil))
      (with-temp-buffer
        (message-insert-formatted-citation-line original-from original-date)
        (goto-char (point-min))
        (while (re-search-forward "\n" nil t)
          (replace-match ""))
        (buffer-string)
        (setq old-citation-line (buffer-string))
        (kill-buffer))
      ;; switch back to the reply-buffer, search the citation line and
      ;; replace it with the new one.
      (save-excursion
        (goto-char (point-min))
        (search-forward old-citation-line)
        (beginning-of-line)
        (kill-line)
        (setq system-time-locale locale)
        (setq message-citation-line-format
              (symbol-value
               (intern (concat locale
                               "-message-citation-line-format"))))
        (message-insert-formatted-citation-line original-from original-date)
        (previous-line)
        (delete-blank-lines)))))

(defun switch-ispell-dictionary ()
   "This function changes the ispell dictionary. If the current
buffer is in mu4e-compose mode, then also change the citation
line."
  (interactive)
  (let ((change (fa/assoc-next-element
                  languages (assoc ispell-current-dictionary languages))))
    (ispell-change-dictionary (car change))
    ;; Try to change citation line when the current buffer is
    ;; mu4e-compose buffer
    (if (eq 'mu4e-compose-mode (buffer-local-value 'major-mode (current-buffer)))
        (change-message-citation-line (cdr change)))))

(global-set-key (kbd "<f6>") 'switch-ispell-dictionary)

;;(add-hook 'mu4e-compose-pre-hook
;;          (defun my-do-compose-stuff ()
;;            "My settings for message composition"
;;            (setq system-time-locale "de_DE.utf8")
;;            (setq message-citation-line-format
;;                  (symbol-value
;;                   (intern (concat system-time-locale
;;                                   "-message-citation-line-format"))))
;;            (turn-on-orgstruct++)
;;            (flyspell-mode)))
