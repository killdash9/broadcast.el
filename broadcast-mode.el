(define-minor-mode broadcast-mode 
  "A minor mode for linking buffers together for simultaneous navigation and 
editing.  When multiple buffers are in broadcast mode, anything done
in one broadcast-mode buffer is replicated in the others.  If a broadcast mode
buffer is not visible, (is not in a window), then it is excluded from broadcasts.
This is to ensure that you don't change something without knowing it.

Broadcast behavior implemented by intercepting commands using `pre-command-hook'
and `post-command-hook'.  Not all commands are broadcast.  It attempts to only
broadcast commands that make sense.  The general strategy is to broadcast any
command that modifies the state collected by `broadcast-get-state'.  We also 
don't rebroadcast anything that modifies the window configuration.

The Kill Ring
Each broadcast mode buffer has a local kill ring that interacts with the default 
kill ring in a sensible manner.  Giving each buffer a local kill ring means you 
can kill and yank as part of your editing, and text is yanked from the local 
kill ring.  Any manipulations to the kill ring outside of a broadcast mode 
buffer are applied to the buffer-local kill rings, allowing you to kill from 
outside a broadcast buffer (or outside emacs) and yank into the broadcast 
buffers.  Any changes to the kill ring made in the primary broadcast buffer 
(that is, the one in the active window) are copied to the default kill ring."
  nil " Broadcast" nil
  (if broadcast-mode
      (progn
        (add-hook 'pre-command-hook 'broadcast-pre nil t)
        (add-hook 'post-command-hook 'broadcast-post t t)
        (make-local-variable 'kill-ring))

    (remove-hook 'pre-command-hook 'broadcast-pre t)
    (remove-hook 'post-command-hook 'broadcast-post t)
    (kill-local-variable 'kill-ring)
    
    (when current-prefix-arg
      (broadcast-command (broadcast-mode 0))
      ;; prefix arg of 0 means turn off all broadcast modes in active windows.
      ;; We have to broadcast the command 
      (message "Broadcast mode disabled for all visible buffers")) 
    ))

(defvar broadcast-suppress nil
  "Used to prevent recursive calling of command hooks")

(defvar broadcast-state nil
  "Holds a snapshot of relevant bits of buffer state in the pre-command-hook 
`broadcast-pre'.  This is checked against the state in the post-command-hook 
`broadcast-post'.  If the state matches, the buffer is considered not to have 
changed significantly and the command is not broadcast to other visible
broadcast-mode buffers.  Snapshots are compared using `equals'.  See 
`broadcast-get-state'")

(defvar broadcast-window-configuration nil
  "Holds a snapshot of the current window configuration when the 
pre-command-hook `broadcast-pre' is called.  It is checked against the current 
window configuration in the post-command-hook `broadcast-post'.  If the window
configurations do not match, the command is not broadcast to other 
visible broadcast-mode buffers.")

(defvar broadcast-transient-mark-mode nil
  "Holds a snapshot of the value of the variable `transient-mark-mode' when 
the pre-command-hook `broadcast-pre' is called.  Since its value may have 
changed by the time the post-command-hook `broadcast-post' is called, the 
`transient-mark-mode' variable is temporarily set via `let' to the snapshot 
value for each of the other visible broadcast-mode buffers.")

(defun broadcast-get-state ()
  "Collects relevant state about the buffer.  Used to detect if a command should
be broadcast."
  (list (buffer-modified-tick)
        (point)
        (mark)
        mark-active
        transient-mark-mode
        rectangle-mark-mode
        kill-ring
        ))

(defun broadcast-pre ()
  "A `pre-command-hook' that is enabled for all broadcast mode buffers.  It 
collects buffer state that is considered in the `post-command-hook' 
`broadcast-post'"
  (unless broadcast-suppress
    (setq broadcast-window-configuration (current-window-configuration))
    (setq broadcast-transient-mark-mode transient-mark-mode)
    (setq broadcast-state (broadcast-get-state))
    ))

(defun broadcast-post ()
  "A `post-command-hook' that broadcasts the command to other visible 
broadcast-mode buffers.  Only certain commands are broadcast.  The general 
strategy is to broadcast any command that modifies the state collected by 
`broadcast-get-state'.  We also don't rebroadcast anything that modifies the 
window configuration."
  (unless broadcast-suppress
    (let ((broadcast-suppress t))
      (when (and
             ;; don't broadcast in isearch mode.
             ;; It would be nice to support this but this is probably tricky
             (not isearch-mode)
             ;; don't broadcast anything that changes the window configuration
             broadcast-window-configuration (compare-window-configurations broadcast-window-configuration (current-window-configuration))
             ;; only broadcast when the state has changed
             (not (equal (broadcast-get-state) broadcast-state)))
        (broadcast-command
         (let ((transient-mark-mode broadcast-transient-mark-mode))
           (condition-case err
               (progn
                 (run-hooks 'pre-command-hook)
                 (call-interactively real-this-command)
                 (run-hooks 'post-command-hook))
             (error (message "%s in %s" (error-message-string err) (buffer-name buffer) ))
             (quit)))))
      ;; set the default (global) kill-ring variable to match our local version
      (setq-default kill-ring kill-ring))))

(advice-add 'kill-new     :around #'broadcast-kill-advice)
(advice-add 'kill-append  :around #'broadcast-kill-advice)
(advice-add 'current-kill :around #'broadcast-kill-advice)
;;(advice-remove 'kill-new     #'broadcast-kill-advice)
;;(advice-remove 'kill-append  #'broadcast-kill-advice)
;;(advice-remove 'current-kill #'broadcast-kill-advice)

(defun broadcast-kill-advice (orig-func &rest args)
  "When a kill ring manipulation happens in a non-broadcast-mode buffer,
repeat that function in all the broadcast-mode buffers since their kill-ring
variable is buffer local"
  (unless broadcast-mode
    (mapcar
     (lambda (buffer)
       (with-current-buffer buffer
         (when broadcast-mode (apply orig-func args))))
     (buffer-list)))
  (apply orig-func args))

(defmacro broadcast-command (body)
  "Evaluates body in all visible broadcast mode buffers."
  `(let ((primary-buffer (current-buffer)))
     (mapcar
      (lambda (buffer)
        (let ((window (get-buffer-window buffer)))
          (when (and window (not (eq buffer primary-buffer)))
            (with-selected-window window
              (when broadcast-mode ,body)))))
      (buffer-list))))

(provide 'broadcast-mode)