;;; window-tamer.el --- Display certain kinds of buffers in certain windows

;; Copyright (C) 2014 Free Software Foundation, Inc.
;;
;; Author:  Telramund <me@privacy.net>
;; Maintainer: Telramund <me@privacy.net>
;; Created: 12 Dec 2014
;; Version: 0.01
;; Keywords

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'window-tamer)

;;; Code:

(defgroup window-tamer nil
  "Work with multiple windows efficiently."
  :group 'convenience)

(defcustom window-tamer/verbose
  t
  "Whether to emit progress messages"
  :type 'boolean
  :group 'window-tamer)

(defcustom window-tamer/goto-window-key-prefix
  "H-"
  "Prefix to build the numeric key bindings for switching to a window,
See `kbd' function for format,"
  :type 'string
  :group 'window-tamer)

(defcustom window-tamer/set-next-window-key-prefix
  "C-H-"
  "Prefix to build the numeric key bindings for choosing next window,
See `kbd' function for format,"
  :type 'string
  :group 'window-tamer)

(defcustom window-tamer/general-prefix-key
  "C-H-"
  "Prefix to build the key bindings for general functions.
See `kbd' function for format,"
  :type 'string
  :group 'window-tamer)

(defcustom window-tamer/window-aliases
  '((main . 1)
    (aux . 2)
    (command . 3))
  "Mnemonic aliases to use in place of window numbers in buffer assignment
configuration. These make it possible to reuse the same configuration on
different window layouts."
  :group 'window-tamer)

(defcustom window-tamer/major-mode-windows
  '((emacs-lisp-mode . main)
    (perl-mode . main)
    (cperl-mode . main)
    (c-mode . main)
    (c++-mode . main)
    (java-mode . main)
    (javascript-mode . main)
    (html-mode . main)
    (xml-mode . main)
    (help-mode . aux)
    (Info-mode . aux)
    (lisp-interaction-mode . aux)
    (compile-mode . aux)
    (shell-mode . aux))
  "Association list mapping major modes to window numbers. Auto-updated by
`window-tamer/assign-major-mode' "
  :group 'window-tamer)

(defcustom window-tamer/buffer-name-regex-windows
  '(("\*Org todo\*" . command))
  "Association list mapping buffer name regular expressions to window aliases
or numbers. Auto-updated by `window-tamer/assign-buffer-name-regex'"
  :group 'window-tamer)

(defvar window-tamer/min-emacs-major-version
  24
  "Minimum major version of Emacs required")


(defun window-tamer/build-keymap ()
  "Returns the mode's keymap"
  (let ((keymap (make-sparse-keymap)))
    ;; Bind numeric keys for per-window functions.
    (dolist (num (mapcar 'number-to-string (number-sequence 1 9)))
      (define-key keymap
        (kbd (concat window-tamer/goto-window-key-prefix num))
        'window-tamer/goto-window-num)
      (define-key keymap
        (kbd (concat window-tamer/set-next-window-key-prefix num))
        'window-tamer/set-next-window-num))
    ;; Bind keys for general functions.
    (dolist (key-binding '(("v" . window-tamer/save)
                           ("m" . window-tamer/assign-major-mode)
                           ("n" . window-tamer/assign-buffer-name-regex)))
      (define-key keymap
        (kbd (concat window-tamer/general-prefix-key (car key-binding)))
        (cdr key-binding)))
    keymap))


(define-minor-mode window-tamer-mode
  "Display buffers in certain windows based on major modes or buffer names"
  :global t
  :group  'window-tamer
  :keymap (window-tamer/build-keymap)

  (unless (>= emacs-major-version window-tamer/min-emacs-major-version)
    (error "window-tamer-mode requires Emacs version %i or later"
           window-tamer/min-emacs-major-version))

  ;; Handle advices and buffer display customisations.
  (let ((hook-def
         '(window-tamer/buffer-auto-window-p
           window-tamer/display-buffer)))
    (if window-tamer-mode
        (progn
          (add-to-list 'display-buffer-alist hook-def)
          (ad-enable-advice 'switch-to-buffer 'after
                            'window-tamer/switch-to-buffer-advice))
      (ad-disable-advice 'switch-to-buffer 'after
                         'window-tamer/switch-to-buffer-advice)
      (setq display-buffer-alist (delete hook-def display-buffer-alist)))
    (ad-activate 'switch-to-buffer)))


(defun window-tamer/get-last-key-num ()
  "Returns the number from numeric key in the last keyboard event"
  (- (event-basic-type last-command-event) 48))


(defun window-tamer/goto-window-num (&optional num)
  "Selects a window by numeric key in the last keyboard event"
  (interactive)
  (select-window (or (window-tamer/get-window-by-num
                      (or num (window-tamer/get-last-key-num)))
                     (error "No such window"))))


(defun window-tamer/unassign-major-mode (&optional major-mode-to-unassign)
  "Removes assignment of a major mode from a window"
  (interactive)
  (unless major-mode-to-unassign
    (setq major-mode-to-unassign
          (intern
           (completing-read
            "Major mode: "
            (mapcar (lambda (item) (format "%S" (car item)))
                    window-tamer/major-mode-windows)))))
  (assq-delete-all major-mode-to-unassign
                   window-tamer/major-mode-windows))


(defun window-tamer/unassign-buffer-name-regex (&optional regex-to-unassign)
  "Removes assignment of a buffer name regular expression from a window"
  (interactive)
  (unless regex-to-unassign
    (setq regex-to-unassign
          (completing-read
           "Regular expression: "
           (mapcar 'regexp-quote
                   (mapcar 'car
                           window-tamer/buffer-name-regex-windows)))))
  (assq-delete-all regex-to-unassign
                   window-tamer/buffer-name-regex-windows))


(defun window-tamer/assign-major-mode ()
  "Updates `window-tamer/major-mode-windows' to map current window to its buffer's major mode"
  (interactive)
  (assq-delete-all major-mode window-tamer/major-mode-windows)
  (let* ((win-num (window-tamer/get-window-num (selected-window)))
         (win-ref (or (car (rassoc win-num window-tamer/window-aliases))
                      win-num)))
    (window-tamer/message "Assigning major mode \"%S\" to window \"%S\""
                          major-mode win-ref)
    (add-to-list 'window-tamer/major-mode-windows (cons major-mode win-ref))))


(defun window-tamer/assign-buffer-name-regex (&optional regex)
  "Updates `window-tamer/buffer-name-regex-windows' to map current window
to buffer name(s) matching a regular expression."
  (interactive)
  (unless regex
    (setq regex (read-string "Regular expression: " (regexp-quote (buffer-name)))))
  (assq-delete-all regex window-tamer/buffer-name-regex-windows)
  (let* ((win-num (window-tamer/get-window-num (selected-window)))
         (win-ref (or (car (rassoc win-num window-tamer/window-aliases))
                      win-num)))
    (window-tamer/message "Assigning regular expression \"%S\" to window \"%S\""
                          regex win-ref)
    (add-to-list 'window-tamer/buffer-name-regex-windows (cons regex win-ref))))


(defun window-tamer/buffer-auto-window-p (buffer &optional action)
  "Returns true if the buffer will be automatically mapped to a window"
  (unless (bound-and-true-p icicle-next-window-for-display-buffer)
    (with-current-buffer buffer
      (if (assoc major-mode window-tamer/major-mode-windows)
          (window-tamer/message "Buffer can be auto-mapped to a window")))))


(defun window-tamer/get-buffer-window (buffer &optional action)
  "Returns an auto-assigned window for the buffer, or nil if there is none"
  (with-current-buffer buffer
    (let ((win-ref
           (catch 'name-match
             (dolist (rule window-tamer/buffer-name-regex-windows)
               (if (string-match (car rule) (buffer-name))
                   (throw 'name-match (cdr rule))))
             (cdr (assoc major-mode window-tamer/major-mode-windows)))))
      (if win-ref
          (progn
            (setq win-num 
                  (if (numberp win-ref)
                      win-ref
                    (or (cdr (assoc win-ref window-tamer/window-aliases))
                        (error "Undefined window reference: %s" win-ref))))
            (nth (- win-num 1) (window-tamer/get-windows-ordered)))))))


(defun window-tamer/display-buffer (buffer alist)
  "A `display-buffer' action displaying the buffer according to major mode"
  (window-tamer/message "Displaying %S in auto-mapped window" buffer)
  (window--display-buffer buffer
                          (window-tamer/get-buffer-window buffer)
                          'reuse))


(defun window-tamer/set-next-window-num (&optional num)
  "Makes the window with given number the next buffer display window"
  (interactive)
  (if (bound-and-true-p icicle-mode)
      (if (setq icicle-next-window-for-display-buffer
                (window-tamer/get-window-by-num
                 (or num (window-tamer/get-last-key-num))))
          (window-tamer/message "Next window will be: %S"
                                icicle-next-window-for-display-buffer)
        (window-tamer/message "No such window"))))


(defun window-tamer/get-windows-ordered ()
  "Returns list of windows in the frame sorted by position"
  (let ((windows-unsorted))
    (walk-windows (lambda (window) (add-to-list 'windows-unsorted window)))
    (sort windows-unsorted 'window-tamer/window-pos<)))


(defun window-tamer/window-pos< (window1 window2)
  "Returns true if first window position is closer to the start"
  (let* ((edges (mapcar 'window-edges (list window1 window2)))
         (x1 (nth 0 (nth 0 edges))) (y1 (nth 1 (nth 0 edges)))
         (x2 (nth 0 (nth 1 edges))) (y2 (nth 1 (nth 1 edges))))
    (or (< y1 y2)
        (and (= y1 y2) (< x1 x2)))))


(defun window-tamer/get-window-num (window)
  "Returns window number in the display order starting from 1"
  (catch 'found
    (let ((num 1))
      (dolist (curr-window (window-tamer/get-windows-ordered))
        (if (eq window curr-window)
            (throw 'found num))
        (setq num (+ 1 num))))))


(defun window-tamer/get-window-by-num (window-num)
  "Returns window with the given number or nil if none such"
  (nth (- window-num 1) (window-tamer/get-windows-ordered)))


(defun window-tamer/save ()
  "Saves current buffer settings to the config file"
  (interactive)
  (customize-save-variable 'window-tamer/major-mode-windows
                           window-tamer/major-mode-windows)
  (window-tamer/message "Window assignments saved"))


(defun window-tamer/message (string &rest rest)
  "Outputs a message respecting the verbosity settings"
  (if window-tamer/verbose
      (apply 'message (concat "window-tamer: " string) rest)))


(defadvice switch-to-buffer (after window-tamer/switch-to-buffer-advice)
  "Choose window based on major mode when switching to a buffer"
  (let* ((buffer (get-buffer (ad-get-arg 0)))
         (force-same-window (ad-get-arg 2))
         (chosen-window (window-tamer/get-buffer-window buffer)))
    (if force-same-window
        (display-buffer-same-window buffer '())
      (when chosen-window
        (window-tamer/message "Window chosen based on buffer: %S" chosen-window)
        (bury-buffer)
        (select-window chosen-window)
        (display-buffer-same-window buffer '())))))

(provide 'window-tamer)
;;; window-tamer.el ends here
