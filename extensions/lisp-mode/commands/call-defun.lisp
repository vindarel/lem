(uiop:define-package :lem-lisp-mode/commands/call-defun
  (:use :cl
        :lem
        :lem-lisp-mode))
(in-package :lem-lisp-mode/commands/call-defun)

(define-key *lisp-mode-keymap* "C-c C-y" 'lisp-call-defun)

(defun define-form-name-p (operator-name)
  (or (member operator-name
              '("defun" "defmacro" "defmethod" "defgeneric" "defsetf")
              :test #'string-equal)
      (alexandria:starts-with-subseq "define-" operator-name)))

(defun symbol-replize (symbol-name package-name)
  (let ((repl-package (lem-lisp-mode/connection:connection-package (current-connection))))
    (if (string-equal repl-package package-name)
        (string-downcase symbol-name)
        (format nil "~(~A::~A~)" package-name symbol-name))))

(defun parse-toplevel-form (point)
  (let ((package-name (buffer-package (point-buffer point) "COMMON-LISP-USER")))
    (lisp-beginning-of-defun point 1)
    (scan-lists point 1 -1)
    (skip-whitespace-forward point)
    (let ((operator-name (symbol-string-at-point point)))
      (cond ((string-equal operator-name "defclass")
             (form-offset point 2)
             (let ((arg-name (symbol-string-at-point point)))
               (values (symbol-replize arg-name package-name)
                       :defclass)))

            ((string-equal operator-name "defstruct")
             (form-offset point 2)
             (let ((arg-name (symbol-string-at-point point)))
               (values (symbol-replize (format nil "make-~A" arg-name) package-name) :defstruct)))

            ((define-form-name-p operator-name)
             (form-offset point 1)
             (skip-whitespace-forward point)
             (cond ((looking-at point "\\(\\s*setf\\S*")
                    (scan-lists point 1 -1)
                    (form-offset point 1)
                    (skip-whitespace-forward point)
                    (values (symbol-replize (symbol-string-at-point point) package-name)
                            :defun-setf))
                   (t
                    (let ((arg-name (symbol-string-at-point point)))
                      (values (symbol-replize arg-name package-name)
                              :defun)))))

            ;; Find a system name as a :keyword or a "string".
            ((string-equal operator-name "defsystem")
             (skip-whitespace-forward point)
             (form-offset point 2)
             (let ((arg-name (lem-lisp-mode/internal::form-string-at-point
                              :syntax-char-fn #'syntax-system-designator-char-p)))
               (when (str:starts-with-p ":" arg-name)
                 (setf arg-name (subseq arg-name 1)))
               (when (str:starts-with-p "\"" arg-name)
                 (setf arg-name (str:trim arg-name :char-bag (list #\"))))
               (values arg-name
                       :defsystem)))
             (t
              (message "Unknown toplevel form: ~a" operator-name))
            )
      )))

(define-command lisp-call-defun () ()
  "Find the top-level form and pre-write a call to it on the REPL.

  - on a defun      -> function call
  - on a setf-defun -> setf-function
  - on a defstruct  -> pre-fill make-[struct name]
  - on a defclass   -> pre-fill (make-instance [class])
  - on a defsystem  -> (ql:quickload \"system name\"])
  "
  (with-point ((point (current-point)))
    (multiple-value-bind (name kind)
        (parse-toplevel-form point)
      (when name
        (ecase kind
          ((:defun)
           (send-string-to-listener (format nil "(~A )" name)
                                    :evaluate nil
                                    :focus t)
           (scan-lists (current-point) -1 -1))
          ((:defun-setf)
           (send-string-to-listener (format nil "(setf (~A ) )" name)
                                    :evaluate nil
                                    :focus t)
           (scan-lists (current-point) -1 -1))
          ((:defstruct)
           (send-string-to-listener (format nil "(~A)" name)
                                    :evaluate nil
                                    :focus t)
           (scan-lists (current-point) -1 -1))
          ((:defclass)
           (send-string-to-listener (format nil "(make-instance '~A)" name)
                                    :evaluate nil
                                    :focus t)
           (scan-lists (current-point) -1 -1))
          ((:defsystem)
           (send-string-to-listener (format nil "(ql:quickload \"~a\")" name)
                                    :evaluate nil
                                    :focus t)
           (scan-lists (current-point) -1 -1)))))))
