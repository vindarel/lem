(uiop:define-package :legit-clone
  (:use :cl
        :lem)
  (:export
   :git-clone-and-open))

(in-package :legit-clone)

(defun run-git-clone (url destination)
  "Clone the project given by URL to DESTINATION.

  Run the command with uiop:run-program, return 3 values: output (string), error-output (string), status code (integer). To handle errors and show them in a Lem popup message, use lem/legit::run-function."
  (let ((cmd (format nil "cd ~a && git clone ~a" destination url)))
    (uiop:run-program cmd
                      :output :string
                      :error-output :string
                      :ignore-error-status t)))

(defun cleanup-url/trailing-# (url)
  (if (str:containsp "#" url)
      (subseq url 0 (position #\# url))
      url))

(defun cleanup-url/trailing-.git (url)
  (if (str:ends-with-p ".git" url)
      (subseq url 0 (- (length url) 4))
      url))

(defun cleanup-url (url)
  "From an URL (string), clean the following:
  - remove a trailing # anchor
  - remove a .git suffix

  Return: the cleaned URL (string).

  If this is not enough, copy-paste a better formated URL ;)"
  (funcall (alexandria:compose
            ;; compose goes from the rightmost function and gives its result to the left.
           #'cleanup-url/trailing-.git
           #'cleanup-url/trailing-#)
          url))

(defun cloned-directory (url destination)
  "Return the directory (string) where we cloned the new project."
  (let ((project-name (alexandria:last-elt (str:split "/" (cleanup-url url)))))
    (str:concat (str:ensure-suffix "/" destination) project-name)))

(define-command git-clone-and-open (url destination) ((:string "URL: ") (:file "Destination: "))
  (lem/legit::run-function
   (lambda ()
     (run-git-clone url destination))

   :on-success
   (lambda ()
     (find-file (cloned-directory url destination)))
   ))
