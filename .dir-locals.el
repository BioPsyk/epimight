((nil .
      ((eval .
             (progn
               (defun ibp/tmux-pane-cmd (pane cmd)
                 "Runs the given shell command in a subshell inside a tmux pane."
                 (interactive)
                 (let* ((resolved-pane (concat "ibp-registry-risk-estimation:" pane))
                        (resolved-cmd (format "'%s'" cmd))
                        (cmd-parts (list "tmux"
                                         "send-keys"
                                         "-t"
                                         resolved-pane
                                         resolved-cmd
                                         "C-m")))
                   (shell-command (format "tmux clear-history -t %s" resolved-pane))
                   (shell-command (mapconcat 'identity cmd-parts " "))))

               (defun get-file-in-project (filename)
                 "Gets absolute path to file inside the project"
                 (interactive "P")
                 (expand-file-name filename (projectile-project-root)))

               (defun ibp/run-scratch ()
                 "Runs scratch file"
                 (interactive)
                 (ibp/tmux-pane-cmd "0.1" "(clear; ./tmp/scratch.sh)"))

               (defun ibp/run-unit-tests ()
                 "Runs all unit tests"
                 (interactive)
                 (ibp/tmux-pane-cmd "0.1" "(clear; ./scripts/run-unit-tests.sh)"))

               (defun ibp/run-system-tests ()
                 "Runs all system tests"
                 (interactive)
                 (ibp/tmux-pane-cmd "0.1" "(clear; ./scripts/run-system-tests.sh)"))

               (defun ibp/run-benchmarks ()
                 "Runs benchmarks"
                 (interactive)
                 (ibp/tmux-pane-cmd "0.1" "(clear; ./scripts/run-benchmarks.sh)"))

               (defun ibp/run-linting ()
                 "Runs linting"
                 (interactive)
                 (ibp/tmux-pane-cmd "0.1" "(clear; ./scripts/run-linting.sh)"))

               (defun ibp/build-package ()
                 "Builds the R package"
                 (interactive)
                 (ibp/tmux-pane-cmd "0.1" "(clear; ./scripts/build-package.sh)"))

               (defun ibp/build-container-image ()
                 "Builds the container image"
                 (interactive)
                 (ibp/tmux-pane-cmd "0.1" "(clear; ./scripts/build-container-image.sh)"))

               (defun ibp/generate-constants ()
                 "Generates constants from database"
                 (interactive)
                 (ibp/tmux-pane-cmd "0.1" "(clear; ./scripts/generate-constants.py)"))

               (defun ibp/kill-dev-env ()
                 "Kills all processes and tmux"
                 (interactive)
                 (ibp/tmux-pane-cmd "0.1" "C-c")
                 (ibp/tmux-pane-cmd "0.1" "tmux kill-session"))

               (global-set-key (kbd "<f1>") 'ibp/run-scratch)
               (global-set-key (kbd "<f2>") 'ibp/run-linting)
               (global-set-key (kbd "<f3>") 'ibp/run-unit-tests)
               (global-set-key (kbd "<f4>") 'ibp/run-system-tests)
               (global-set-key (kbd "<f5>") 'ibp/run-benchmarks)
               (global-set-key (kbd "<f6>") 'ibp/build-package)
               (global-set-key (kbd "<f7>") 'ibp/build-container-image)
               (global-set-key (kbd "<f8>") 'ibp/generate-constants)
               (global-set-key (kbd "<f12>") 'ibp/kill-dev-env)

               )))))
