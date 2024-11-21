(uiop:define-package :lem/keymenu
    (:use :cl :lem)
  (:documentation "Goal: display a window on top of the others,
  create parameters to display inside it,
  define keybindings that allow to change the parameters' value."))

;;; After loading this file,
;;; create test variables (see define-toggle-command for *a* and *b*).

(in-package :lem/keymenu)


(define-minor-mode keymenu-mode
    (:name "Menu"
     :keymap *keymenu-keymap*)
  (setf (not-switchable-buffer-p (current-buffer)) t))

(define-key *keymenu-keymap* "q" 'keymenu-quit)
(define-key *keymenu-keymap* "C-c C-k" 'keymenu-quit)

;; commands
;; (define-key *keymenu-keymap* "s" (lambda () (message "pressed s")))

;; quit
(define-key *keymenu-keymap* "Return" 'keymenu-select)

;; navigation
(define-key *keymenu-keymap* 'next-line 'keymenu-next)


;;;
;;; The two windows pane.
;;;
(define-attribute filename-attribute
  (t :foreground :base0D))

(define-attribute highlight
  (t :background :base0D))

(defvar *collector*)

(defclass collector ()
  ((buffer :initarg :buffer
           :reader collector-buffer)
   (count :initform 0
          :accessor collector-count)))

(defvar *peek-window*)
(defvar *parent-window*)

(defclass peek-window (floating-window) ())

(defmethod lem-core::%delete-window :before ((window peek-window))
  (finalize-keymenu))

(defmethod compute-window-list ((current-window peek-window))
  (list *peek-window*))

(defvar *is-finalzing* nil)

(defun finalize-keymenu ()
  (unless *is-finalzing*
    (let ((*is-finalzing* t))
      (finalize-highlight-overlays)
      (setf (current-window) *parent-window*)
      (delete-window *peek-window*))))

(defun set-move-function (start end move-function)
  (with-point ((end start))
    (character-offset end 1)
    (put-text-property start end :move-marker t))
  (put-text-property start end :move-function move-function))


(defun get-move-function (point)
  (with-point ((point point))
    (line-start point)
    (text-property-at point :move-function)))

(defun start-move-point (point)
  (buffer-start point)
  (unless (text-property-at point :move-marker)
    (next-move-point point)))

(defun next-move-point (point)
  "Find the next point (line) with a marker.
  This is how we distinguish between simple text, and meaningful text."
  (when (text-property-at point :move-marker)
    (next-single-property-change point :move-marker))
  (next-single-property-change point :move-marker))

(defun previous-move-point (point)
  (when (text-property-at point :move-marker)
    (previous-single-property-change point :move-marker))
  (previous-single-property-change point :move-marker))

(defun next-header-point (point)
  "Find the next point (line) with a header marker."
  (when (text-property-at point :header-marker)
    (next-single-property-change point :header-marker))
  (next-single-property-change point :header-marker))

(defun previous-header-point (point)
  "Find the previous point (line) with a header marker."
  (when (text-property-at point :header-marker)
    (previous-single-property-change point :header-marker))
  (previous-single-property-change point :header-marker))

(defparameter *x-margin* 20)
(defparameter *width* (- (display-width) (* 2 *x-margin*)))
(defparameter *y-margin* 10)
(defparameter *height* (- (display-height) (* 2 *y-margin*)))

(defun make-square-window (buffer)
  (let* ((x-margin 4)
         (y-margin 2)
         (width (or *width* (- (floor (display-width) 2) 2 x-margin)))
         (height (or *height* (- (display-height) 2 (* 2 y-margin))))
         (peek-window (make-instance 'peek-window
                                     :buffer buffer
                                     :x (+ 1 *x-margin*)
                                     :y (+ 1 *y-margin*)
                                     :width width
                                     :height height
                                     :use-border t)))
    (list peek-window)))

(defun display (collector &key (minor-mode 'keymenu-mode))
  (when (boundp '*peek-window*)
    (delete-window *peek-window*))

  (destructuring-bind (peek-window)
      (make-square-window (collector-buffer collector))

    (unless (boundp '*parent-window*)
      (setf *parent-window* (current-window)))

    (setf *peek-window* peek-window)

    (setf (current-window) peek-window)

    (funcall minor-mode t)
    ;; aka:
    ;; (keymenu-mode t)

    (start-move-point (buffer-point (collector-buffer collector)))
    (print "do display")))

(defun make-keymenu-buffer (&key (name "*keymenu*"))
  "Get or create a buffer of name NAME. By default, use a `*keymenu*' buffer.
  This is where we will display legit information (status…)."
  (let ((buffer (make-buffer name
                             :temporary t
                             :enable-undo-p t
                             :directory (uiop:getcwd))))
    (setf (variable-value 'line-wrap :buffer buffer) nil)
    buffer))

(defun call-with-collecting-sources (function &key read-only buffer (minor-mode 'keymenu-mode))
  "Initialize variables to display things on a legit buffer.

  BUFFER: either :status or :commits-log.
  READ-ONLY: boolean.
  MINOR-MODE: the minor mode to activate after we displayed things in the buffer. Defaults to the main keymenu-mode. The mode is activated with:

    (keymenu-mode t)
  or
    (funcall minor-mode t)"
  (let* ((*collector* (make-instance 'collector
                                     :buffer
                                     (make-keymenu-buffer
                                      :name
                                      (case buffer
                                        (:status "*keymenu*")
                                        (:commits-log "*legit-commits-log*")
                                        (t (error "Unknown buffer name to display legit data: ~a" buffer))))))
         (point (buffer-point (collector-buffer *collector*))))
    (declare (ignorable point))
    (funcall function *collector*)
    (when read-only
      (setf (buffer-read-only-p (collector-buffer *collector*)) t))
      (display *collector* :minor-mode minor-mode)))

(defmacro with-collecting-sources ((collector &key (buffer :status)
                                                (read-only t)
                                                (minor-mode 'keymenu-mode))
                                   &body body)
  "Top-level macro that prepares a buffer to print stuff on and activates a minor-mode.

  Then see `with-appending-source' and `collector-insert'."
  `(call-with-collecting-sources (lambda (,collector)
                                   (declare (ignorable ,collector))
                                   ,@body)
                                 :buffer ,buffer
                                 :minor-mode ,minor-mode
                                 :read-only ,read-only))

(defun call-with-appending-source (insert-function
                                   move-function
                                   visit-file-function)
  (declare (ignorable visit-file-function))
  (let ((point (buffer-point (collector-buffer *collector*))))
    (with-point ((start point))
      (funcall insert-function point)
      (unless (start-line-p point)
        (insert-string point (string #\newline) :read-only t))
      (set-move-function start point move-function)
    (incf (collector-count *collector*)))))

(defmacro with-appending-source ((point &key move-function
                                             visit-file-function
                                          ) &body body)
  "Macro to use inside `with-collecting-sources' to print stuff.

  Save the lambda functions :move-function etc to their corresponding string properties.

  A keybinding is associated to these functions.
  They will dig up the lambda functions associated with these markers and run them.

  Devel note 2024: the key arguments move-function, visit-file-function etc
  are now badly named. They should represent a function tied to an action:
  - what to do when the point moves on this line (this is currently move-function to show diffs)
  - what to do on Enter (this is currently visit-file-function)
  - what to do on the `s` keybinding (currently stage-function)
  etc

  Not everything represented on legit status represents a file.
  We now use :visit-file-function and :stage-function to have actions on stashes."
  `(call-with-appending-source (lambda (,point) ,@body)
                               ,move-function
                               ,visit-file-function))

(defun collector-insert (s &key (newline t) header)
  (let ((point (buffer-point (collector-buffer *collector*))))
    (with-point ((start point))
      (character-offset start 1)
      (insert-string point s :read-only t)
      (when header
        (put-text-property start point :header-marker t))
      (when newline
        (insert-string point (string #\newline) :read-only t)))))

;;;
(define-attribute match-line-attribute
  (t :background :base02))

(defmethod execute :after ((mode keymenu-mode) command argument)
  "After a command is run in this mode, apply an effect.

  In the case of `keymenu-mode', it is run after `keymenu-next',
  in order to show the file content on the right window.

  The method is to subclass for all legit modes."
  (when (eq (current-window) *peek-window*)
    (print "do execute")))

(defun highlight-matched-line (point)
  (let ((overlay (make-line-overlay point 'highlight)))
    (start-timer (make-timer (lambda ()
                               (delete-overlay overlay))
                             :name "highlight-matched-line")
                 300)))


(define-command keymenu-select () ()
  "Run the action stored in the :visit-file-function marker. Bound to Enter.

  By default, this function works on files:
  - execute the lambda function from the marker,
  - expect its return value is a file name
  - and visit the file, in the context of the current VCS.

  It is possible to run actions not tied to files, for example do
  something when pressing Enter on a line representing a commit stash.
  The lambda function needs to return nil or (values)."
  (print "select something"))

(define-command keymenu-next () ()
  "Find the next line with a :move-marker text property.

  After finding it, our :after method of `execute' is run, to apply an effect, showing the new diff on the right."
  (next-move-point (current-point)))

(define-command keymenu-next-header () ()
  (next-header-point (current-point)))

(define-command keymenu-previous-header () ()
  (previous-header-point (current-point)))

(define-command keymenu-previous () ()
  (previous-move-point (current-point)))

(defparameter *menu-parameters* (list 'a :a))

(defun %keymenu-quit ()
  "Delete the two side windows."
  ;; Shall we delete keybindings?
  (setf (current-window) *parent-window*)
  (start-timer
   (make-idle-timer (lambda ()
                      (delete-window *peek-window*)))
   0))

(define-command keymenu-quit () ()
  "Quit"
  (%keymenu-quit)
  (message (apply 'format nil "menu values? a is ~s, b is ~s" (%show-debug-values))))


;;;
(defvar *highlight-overlays* '())

(defun set-highlight-overlay (point)
  (let ((overlay (make-line-overlay point (ensure-attribute 'match-line-attribute))))
    (push overlay *highlight-overlays*)
    (setf (buffer-value (point-buffer point) 'highlight-overlay) overlay)))

(defun get-highlight-overlay (point)
  (buffer-value (point-buffer point) 'highlight-overlay))

(defun update-highlight-overlay (point)
  (let ((overlay (get-highlight-overlay point)))
    (cond (overlay
           (move-point (overlay-start overlay) point)
           (move-point (overlay-end overlay) point))
          (t
           (set-highlight-overlay point)))))

(defun finalize-highlight-overlays ()
  (dolist (overlay *highlight-overlays*)
    (buffer-unbound (overlay-buffer overlay) 'highlight-overlay)
    (delete-overlay overlay))
  (setf *highlight-overlays* '()))

;;;
;;; Now use all of the above to create a top-level keymenu.
;;;

(define-command print-text () ()
  (with-collecting-sources (collector :read-only nil :minor-mode 'keymenu-mode)
    (with-appending-source (point)
      (insert-string point "hello a"))))

(defclass parameter ()
  ((name :initarg :name :initform nil :accessor parameter-name)
   (variable :initarg :variable :initform nil :accessor parameter-variable)
   ;; (default-value :initarg :default-value :initform nil :accessor parameter-default-value)
   (keybinding :initarg :keybinding :initform nil :accessor parameter-keybinding)
   (command :initarg :command :initform nil :accessor parameter-command)
   (docstring :initarg :docstring :initform "" :accessor parameter-docstring)))

#++
(defclass-std:print-object/std parameter)

(defun make-parameter (&rest rest)
  (apply 'make-instance 'parameter rest))

(defparameter *a* nil)
(defparameter *my-a-parameter*
  (make-instance 'parameter
                  :name "a"
                  :variable '*a*
                  :keybinding "C-a"
                  :command :toggle
                  :docstring "Set a if you want to test the menu and update the screen."))

#++
(define-toggle-defun *my-a-parameter*)
#++
(define-toggle-command *my-a-parameter*)


(defparameter *b* nil)
(defparameter *my-b-parameter*
  (make-instance 'parameter
                  :name "b"
                  :variable '*b*
                  :keybinding "C-b"
                  :command :toggle
                   :docstring "Set b too."))

#++
(define-toggle-defun *my-b-parameter*)
#++
(define-toggle-command *my-b-parameter*)

(defun %show-debug-values ()
  (list *a* *b*))

(defparameter *menu-parameters* (list *my-a-parameter* *my-b-parameter*)
  "Associate a good-looking name to a parameter and a keybinding to change it.")

(defun toggle-parameter (param)
  (let ((var (parameter-variable param))
        (value (eval (parameter-variable param))))
    (eval `(setf ,var (not ,value)))))

(defmacro define-toggle-defun (param)
  "Define a defun-<param name> to test toggling of values."
  (let ((fn-name (alexandria:symbolicate "TOGGLE-" (str:upcase
                                                    (parameter-name
                                                     (eval param))))))
    `(defun ,fn-name ()
       (toggle-parameter ,param))))

(defmacro define-toggle-command (param)
  "Define a command and a keybinding after this parameter name,
  that toggle this parameter's variable value,
  so that we can toggle the values from the menu with keybindings."
  (let ((fn-name (alexandria:symbolicate "%KEYMENU-TOGGLE-" (str:upcase
                                                                (parameter-name
                                                                 (eval param))))))
    `(progn
       (define-command ,fn-name () ()
         (toggle-parameter ,param)
         ;; re-draw everything.
         (create-menu))

       (define-key *keymenu-keymap* (parameter-keybinding ,param) ',fn-name)
       )))

(defun create-menu (&key
                      (title "test menu")
                      intro
                      (outro "Quit with (q).")
                      (parameters *menu-parameters*))

  ;; Write content.
  (with-collecting-sources (collector :read-only nil
                                      :minor-mode 'keymenu-mode)
    ;; (if we don't specify the minor-mode, the macro arguments's default value will not be found)
    (collector-insert title :header t)
    (when intro
      (collector-insert (format nil "~&~%~a" intro)))
    (loop for param in parameters
          do (collector-insert
              (format nil "~&~%~a: ~a (~a)"
                      (parameter-name param)
                      (eval (parameter-variable param))
                      (parameter-keybinding param)))
             (when (parameter-docstring param)
               (collector-insert (format nil "~&~a" (parameter-docstring param)))))

    (when outro
      (collector-insert (format nil "~&~%~a" outro)))
    ))

(define-key *global-keymap* "C-x m" 'keymenu-test)

(define-command keymenu-test () ()
  (create-menu))
