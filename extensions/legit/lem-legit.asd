
(defsystem "lem-keymenu"
  :serial t
  :depends-on ("lem")
  :components ((:module "./"
                :components ((:file "keymenu")))))

(defsystem "lem-legit"
  :serial t
  :depends-on ("lem" "lem-patch-mode" "lem-yaml-mode" "lem-markdown-mode")
  :components ((:module "./"
                :components ((:file "porcelain")
                             (:file "porcelain-git")
                             (:file "porcelain-hg")
                             (:file "porcelain-fossil")
                             ;; (:file "keymenu")
                             (:file "legit-common")
                             (:file "peek-legit")
                             (:file "legit")
                             (:file "legit-rebase")
                             (:file "legit-commit")))
               (:module "scripts"
                :components ((:static-file "dumbrebaseeditor.sh")))))
