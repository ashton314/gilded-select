;;; gilded-select.el --- Minor enhancements to the excellent Selectrum package -*- lexical-binding: t -*-

;; Copyright © 2020 Ashton Wiersdorf

;; Author: Ashton Wiersdorf <ashton.wiersdorf@pobox.com>
;; Created: 20 Oct 2020
;; Version: 0.0.4
;; Package-Requires: ((emacs "25.1") (selectrum "2.0"))
;; Keywords: extensions selectrum bindings
;; SPDX-License-Identifier: MIT
;; Homepage: https://github.com/ashton314/gilded_select

;;; Commentary:

;; This uses Selectrum, but also displays the key the command is
;; bound to, as well as colorizes the currently active major/minor
;; modes.

;; Please see the source repository at
;; https://github.com/ashton314/gilded_select for more information on
;; this project. See also the excellent Selectrum package located at
;; https://github.com/raxod502/selectrum

;;; Code:

;;;; Libraries

(require 'selectrum)

;;;; Faces

(defface gilded-minor-mode-active
  '((t (:foreground "green")))
  "Face used to highlight active minor modes in the minibuffer."
  :group 'gilded-faces)

(defface gilded-minor-mode-inactive
  '((t (:foreground "red")))
  "Face used to highlight inactive minor modes in the minibuffer.
Note: only shows up for modes that were active in the past."
  :group 'gilded-faces)

;;;; Variables

(defvar gilded-command-keybinding-hash (make-hash-table) "Cache of commands to the keys they are bound to.")

(defvar gilded--old-M-x-function nil "Previous function bound to `M-x'.")

;;;; Functions

(defun gilded-rehash-key-bindings ()
  "Rebuild `gilded-command-keybinding-hash' so that calling
`execute-extended-command' will show the proper keybindings next
to the functions they're bound to."
  (interactive)
  (let ((start-time (current-time)))
    (obarray-map
     (lambda (sym)
       (when (and (commandp sym) (where-is-internal sym))
	 (puthash sym (format "%s %s" (symbol-name sym)
			      (gilded-keybind-propertize (key-description (car (where-is-internal sym)))))
		  gilded-command-keybinding-hash)
	 ))
     obarray)
    (message (format "Finished building keybinding cache; processed %s obarray entries in %s seconds"
		     (length obarray)
		     (time-to-seconds (time-subtract (current-time) start-time))))))

(defun gilded-keybind-propertize (str)
  "Function used to format the keybinding annotation. Receives
the keybinding without any frills."
  (format "(%s)" (propertize str 'face 'font-lock-doc-face)))

(defun gilded-string-gen-times (n str acc)
  (if (= n 0) acc (gilded-string-gen-times (- n 1) str (cons str acc))))

(defun gilded-prompt-string (prefix)
  "Given a prefix argument, builds a prompt string for `gilded-mx'."
  (format "%sM-x "
	  (if (and prefix (listp prefix) (not (= (car prefix) 0)) (memq (car prefix) '(4 16 32 128)))
	      (apply #'concat (gilded-string-gen-times (log (car prefix) 4) "C-u " '()))
	    (if prefix (format "(%s) " prefix) ""))))

;;;###autoload
(defun gilded-mx (prefix)
  "Like `execute-extended-command', but with fancy annotations."
  (interactive "P")
  (let ((current-prefix-arg prefix))
    (call-interactively
     (intern				; This strips off the annotations since they're all just propertized on
      (car (split-string                ; Remove the keybind annotation if present
	    (completing-read
	     (gilded-prompt-string prefix)
	     (let ((modes (make-hash-table :test #'equal))
		   (keymap (make-hash-table :test #'equal))
		   (cmds nil))
	       (map-do
		(lambda (var _)
		  (when (boundp var)
		    (puthash
		     (or (get var :minor-mode-function) var)
		     (propertize
		      (symbol-name var)
		      'face
		      (if (symbol-value var)
			  'gilded-minor-mode-active
			'gilded-minor-mode-inactive))
		     modes)))
		minor-mode-alist)

	       (obarray-map
		(lambda (sym)
		  (when (commandp sym)
		    (push
		     (or (gethash sym modes)
			 (gethash sym gilded-command-keybinding-hash)
			 (symbol-name sym))
		     cmds)))
		obarray)
	       cmds)
	     nil
	     'require-match) " ")))
     'record)))

;; Rehash all the keybindings when we change to a new mode
;; (add-hook 'after-change-major-mode-hook 'gilded-rehash-key-bindings)

;;;###autoload
(define-minor-mode gilded-select-mode
  "Minor mode to use gilded-select for `M-x'

Enables `selectrum-mode' and sets `M-x' to run `gilded-mx'. Also
adds a hook to rehash the keybindings."
  :init-value t
  :lighter " Au"
  :global t
  :keymap (let ((gilded-map (make-sparse-keymap)))
	    (define-key gilded-map (kbd "M-x") 'gilded-mx)
	    gilded-map)
  (selectrum-mode +1)
  (add-hook 'after-init-hook 'gilded-rehash-key-bindings))

;;;; Postlude

(provide 'gilded-select)

;;; gilded-select.el ends here
