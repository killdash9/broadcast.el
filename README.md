broadcast-mode.el
=================

A minor mode for linking buffers together for simultaneous navigation and 
editing.  

When multiple buffers are in broadcast mode, anything done
in one broadcast-mode buffer is replicated in the others.  If a broadcast mode
buffer is not visible, (is not in a window), then it is excluded from broadcasts.
This is to ensure that you don't change something without knowing it.  

Broadcast behavior implemented by intercepting commands using `pre-command-hook'
and `post-command-hook'.  Not all commands are broadcast.  It attempts to only
broadcast commands that make sense.  The general strategy is to broadcast any
command that modifies the state collected by `broadcast-get-state'.  We also 
don't rebroadcast anything that modifies the window configuration.

The Kill Ring
-------------
Each broadcast mode buffer has a local kill ring that interacts with the default 
kill ring in a sensible manner.  Giving each buffer a local kill ring means you 
can kill and yank as part of your editing, and text is yanked from the local 
kill ring.  Any manipulations to the kill ring outside of a broadcast mode 
buffer are applied to the buffer-local kill rings, allowing you to kill from 
outside a broadcast buffer (or outside emacs) and yank into the broadcast 
buffers.  Any changes to the kill ring made in the primary broadcast buffer 
(that is, the one in the active window) are copied to the default kill ring.

Known Limitations
-------------
Using the undo command in this mode is known to cause buffers to get out of sync.
