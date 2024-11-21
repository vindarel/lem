(in-package :lem/keymenu)

;; test data.
;;
;; We can't have the defparameter and use it in the define-toggle-command
;; in the same file.
;; We can load this file because the parameters are defined earlier in keymenu.lisp

(defparameter *a* nil)
(defparameter *my-a-parameter*
  (make-instance 'parameter
                 :name "a"
                 :variable '*a*
                 :keybinding "C-a"
                 :command :toggle
                 :docstring "Set a if you want to test the menu and update the screen."))

(define-toggle-defun *my-a-parameter*)
(define-toggle-command *my-a-parameter*)


(defparameter *b* nil)
(defparameter *my-b-parameter*
  (make-instance 'parameter
                 :name "b"
                 :variable '*b*
                 :keybinding "C-b"
                 :command :toggle
                 :docstring "Set b too."))

(define-toggle-defun *my-b-parameter*)
(define-toggle-command *my-b-parameter*)

(defun %show-debug-values ()
  (list *a* *b*))

(defparameter *menu-parameters* (list *my-a-parameter* *my-b-parameter*)
  "Associate a good-looking name to a parameter and a keybinding to change it.")
